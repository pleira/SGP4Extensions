#!/usr/bin/env python3
"""Download current TLEs from celestrak.org and generate a python3-sgp4 reference.

Writes two files under tests/src/test/resources:

* celestrakTLEs.txt: a snapshot of the downloaded TLEs (name line + two
  TLE lines per satellite), so the cross-check test is deterministic.
* celestrakReference.csv: satnum,t,x,y,z,vx,vy,vz rows from the system
  python3-sgp4 library (TEME, km and km/s), one row per propagation time.

Only near-Earth satellites are kept (mean motion >= 6.4 rev/day, i.e.
period <= 225 min): the deep space algorithm is not implemented in this
project. Only rows for which the reference implementation reports no
error are included; the Scala cross-check test propagates exactly those
rows and fails if it cannot.

Re-run this script any time to refresh the snapshot with current TLEs:

    python3 scripts/gen-celestrak-reference.py
"""

import io
import os
import time
import urllib.request

from sgp4.api import Satrec

HERE = os.path.dirname(os.path.abspath(__file__))
TLE_FILE = os.path.join(HERE, "..", "tests", "src", "test", "resources", "celestrakTLEs.txt")
REF_FILE = os.path.join(HERE, "..", "tests", "src", "test", "resources", "celestrakReference.csv")

# A few groups with varied inclinations and drag; a handful of satellites each.
GROUPS = ["stations", "starlink", "oneweb", "iridium-NEXT", "planet", "orbcomm", "spire"]
PER_GROUP = 5
CELESTRAK_URL = "https://celestrak.org/NORAD/elements/gp.php?GROUP={}&FORMAT=tle"

# propagation times in minutes from each TLE's epoch
TIMES = [0, 120, 240, 360, 480, 600, 720, 840, 960, 1080, 1200, 1320, 1440,
         2160, 2880, 4320, 8640]

# near-Earth limit: period <= 225 min
MIN_MEAN_MOTION = 1440.0 / 225.0


def fetch_group(name, attempts=3):
    url = CELESTRAK_URL.format(name)
    headers = {"User-Agent": "SGP4Extensions cross-check (github.com/pleira/SGP4Extensions)"}
    for attempt in range(attempts):
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=60) as response:
                return response.read().decode("utf-8")
        except urllib.error.HTTPError:
            if attempt == attempts - 1:
                raise
            time.sleep(10)  # celestrak throttles rapid successive requests


def parse_tles(text):
    """Yield (name, line1, line2) triples from a celestrak group file."""
    lines = [l.rstrip("\r\n") for l in io.StringIO(text)]
    i = 0
    while i < len(lines) - 1:
        if lines[i].startswith("1 ") and lines[i + 1].startswith("2 "):
            # no name line
            yield ("", lines[i], lines[i + 1])
            i += 2
        elif i + 2 < len(lines) and lines[i + 1].startswith("1 ") and lines[i + 2].startswith("2 "):
            yield (lines[i].strip(), lines[i + 1], lines[i + 2])
            i += 3
        else:
            i += 1


def mean_motion(line2):
    try:
        return float(line2[52:63])
    except ValueError:
        return 0.0


def main():
    tles = []
    for group in GROUPS:
        try:
            text = fetch_group(group)
        except Exception as e:  # noqa: BLE001 - a missing group should not abort the run
            print(f"skipping group {group}: {e}")
            continue
        kept = 0
        for name, line1, line2 in parse_tles(text):
            if mean_motion(line2) < MIN_MEAN_MOTION:
                continue
            if len(line2) < 69:
                continue
            if len(line1) < 69:
                continue
            sat = Satrec.twoline2rv(line1, line2)
            tles.append((name, line1, line2))
            kept += 1
            if kept >= PER_GROUP:
                break
        print(f"group {group}: kept {kept} satellites")

    with open(TLE_FILE, "w") as f:
        for name, line1, line2 in tles:
            f.write(f"{name}\n{line1}\n{line2}\n")

    rows = ["satnum,t,x,y,z,vx,vy,vz"]
    for name, line1, line2 in tles:
        sat = Satrec.twoline2rv(line1, line2)
        for t in TIMES:
            jd = sat.jdsatepoch + t // 1440
            fr = sat.jdsatepochF + (t % 1440) / 1440.0
            err, r, v = sat.sgp4(jd, fr)
            if err != 0:
                continue
            rows.append(
                "%d,%.10f,%.10f,%.10f,%.10f,%.10f,%.10f,%.10f"
                % (sat.satnum, t, r[0], r[1], r[2], v[0], v[1], v[2])
            )

    with open(REF_FILE, "w") as f:
        f.write("\n".join(rows) + "\n")
    print(f"wrote {len(tles)} satellites to {TLE_FILE}")
    print(f"wrote {len(rows) - 1} reference rows to {REF_FILE}")


if __name__ == "__main__":
    main()
