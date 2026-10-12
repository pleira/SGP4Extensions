#!/usr/bin/env python3
"""Generate a propagation reference from the system python3-sgp4 library.

Reads tests/src/test/resources/nearTLEs.txt (TLE pairs, optionally preceded by
'#' comment lines; the second line of each pair carries three trailing numbers
used as start time, end time and step, in minutes) and writes a CSV with
position/velocity in km and km/s (TEME frame) for each time in the grid.

Only rows for which the reference implementation reports no error are emitted.

Usage:
    python3 scripts/gen-python-sgp4-reference.py
"""

import os
import sys

from sgp4.api import Satrec

HERE = os.path.dirname(os.path.abspath(__file__))
TLE_FILE = os.path.join(HERE, "..", "tests", "src", "test", "resources", "nearTLEs.txt")
OUT_FILE = os.path.join(HERE, "..", "tests", "src", "test", "resources", "pythonSgp4Reference.csv")


def read_tles(path):
    lines = [line.rstrip("\n") for line in open(path)]
    tles = []
    i = 0
    while i < len(lines):
        line1 = lines[i]
        i += 1
        if line1.startswith("#") or not line1.strip():
            continue
        line2 = lines[i]
        i += 1
        if line2.startswith("#"):
            continue
        start, end, step = (float(tok) for tok in line2.split()[-3:])
        tles.append((line1, line2, start, end, step))
    return tles


def times(start, end, step):
    count = int(round((end - start) / step)) + 1
    return [start + step * i for i in range(count)]


def main():
    rows = ["satnum,t,x,y,z,vx,vy,vz"]
    for line1, line2, start, end, step in read_tles(TLE_FILE):
        sat = Satrec.twoline2rv(line1, line2)
        for t in times(start, end, step):
            jd = sat.jdsatepoch
            fr = sat.jdsatepochF + t / 1440.0
            carry = int(fr)
            jd += carry
            fr -= carry
            err, r, v = sat.sgp4(jd, fr)
            if err != 0:
                continue
            rows.append(
                "%d,%.10f,%.10f,%.10f,%.10f,%.10f,%.10f,%.10f"
                % (sat.satnum, t, r[0], r[1], r[2], v[0], v[1], v[2])
            )
    with open(OUT_FILE, "w") as f:
        f.write("\n".join(rows) + "\n")
    print("wrote %d rows to %s" % (len(rows) - 1, OUT_FILE))


if __name__ == "__main__":
    sys.exit(main())
