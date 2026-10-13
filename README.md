SGP4Extensions
==============

Scala project to try different SGP4 algorithm implementations. SGP4 is used to predict orbits from TLE data. The Deep Space algorithm, which contains lunar-solar perturbations, is not implemented. 

Most of the code is based in the work done by Vallado, documented in [SpaceTrackReport #3](https://celestrak.com/NORAD/documentation/). To verify the results, the cpp [public code](http://celestrak.com/publications/AIAA/2006-6753/) has been taken. Also, I have looked into the TLE processing done in the [Orekit](http://www.orekit.org) project.
My initial aim is to introduce an alternative implementation for the calculation of the perturbations suggested by [Martin Lara](http://arxiv.org/pdf/1407.8076.pdf). 

This implementation is also an exercise of having cleaner code written in idiomatic Scala 3. That means, the physical models and transformations should appear clearer in the code. 

The numerical library Spire is used, and the propagation is parameterized on the numeric type F through Spire's type classes. The right class for propagation is **Double**: TLE data quantizes the elements to a few decimal places, which keeps even the conflictive points of the theory (low inclinations, and the critical inclination at 63.4 deg, where the perigee rate vanishes) well conditioned in Double; the achievable accuracy is limited by the TLE data and by the SGP4 model itself, not by round off. Higher precision cannot improve a propagation. That said, the parameterization keeps the door open to other numeric types for **round off analysis**: propagating with Spire's Real measures the accumulated round off of the Double pipeline, which is of interest in conjunction experiments, where a small position error shifts the time of a close approach. That analysis, not propagation accuracy, is the use case of the numeric type parameterization, at the expense of a much longer running time. 

The SGP4 pipeline
-----------------

All propagator variants share the same front end:

    TLE -> SGP elements in context (SGPElemsConversions)
        -> Brouwer/Lane secular corrections (BrouwerLaneSecularCorrections)
        -> periodic corrections
        -> position and velocity in the TEME frame

The secular part propagates the mean elements in time with the J2, J3 and J4
gravitational terms plus Lane's exponential atmospheric drag. The variants
differ only in the periodic corrections, the step that turns the mean elements
at time t into osculating position and velocity:

| Propagator | Long-period corrections | Short-period corrections |
|---|---|---|
| `SGP4Vallado` | Vallado's fixups in Lyddane's variables, Kepler's equation solved in Lyddane form | in Special Polar Nodals |
| `SGP4ValladoLong` | as SGP4Vallado, plus extra long-period terms | in Special Polar Nodals |
| `SGP4PN` | computed directly in Special Polar Nodals | in Special Polar Nodals |
| `SGP4Lara` | first-order terms in Lara's non-singular variables | in Lara's non-singular variables |

A description of the near-Earth SGP4 algorithm is in [sgp4.md](sgp4.md); the
design rationale of this implementation is in [doc/proposal.md](doc/proposal.md).

Lara's derivation changes
-------------------------

Vallado's reference algorithm evaluates the long-period corrections in
Lyddane's variables: element combinations introduced so that Brouwer's periodic
corrections can be evaluated without the perigee and node singularities. The
correction terms in current SGP4 implementations are the closed-form fixups of
the C++ reference code.

The alternative suggested by [Martin Lara](http://arxiv.org/pdf/1407.8076.pdf)
derives the first-order corrections directly in a non-singular variable set
(ψ, ξ, χ, r, R, Θ): the node and the argument of latitude are combined into
ψ = Ω + θ, and ξ = s sin θ, χ = s cos θ with s = sin i. `SGP4Lara` transforms
the secular state into these variables, applies the corrections there, and
transforms back to polar nodals.

Two properties of the formulation are visible in the code:

* each family of corrections is proportional to one small parameter:
  ϵ3 = (J3/J2)/2p for the J3 long-period terms and ϵ2 = −J2/4p² for the J2
  short-period terms (`LaraFirstOrderCorrections`), so the structure of the
  perturbation theory reads directly from the code;
* the inclination is never a propagated variable: N = Θ cos i is evaluated once
  from the secular state and is all that the polar nodal <-> non-singular
  round trip needs (`spn2lns`, `lns2spn`).

The comparison tests verify the Lara route against Vallado's reference outputs
and against the polar-nodal route, with tight tolerances.

Project layout
--------------

The package structure mirrors the python sgp4 package:

* `sgp4.io`: TLE parsing (python: `sgp4.io` with `twoline2rv`)
* `sgp4.earth_gravity`: the WGS-72/84 constants (python: `sgp4.earth_gravity` with `wgs72`, `wgs84`)
* `sgp4.coord`: coordinate sets and transformations (no python equivalent)
* `sgp4.propagation`: the secular corrections model and the periodic correction
  traits (python: `sgp4.propagation`)
* `sgp4.model`: the propagator variants (python: `sgp4.model` with `Satrec`)

By module:

* `core`: the SGP4 library
* `tests`: the reference test suite (Vallado's C++ outputs, the python3-sgp4
  cross-check, algorithm-to-algorithm comparisons) and the TLE fixtures
* `reports`: the `sgp4.Main` command line propagator, and the comparison report programs
* `benchmarks`: the JMH micro-benchmarks


The sgp4.Main command line propagator
-------------------------------------

`reports/runMain sgp4.Main` propagates a two line element set given as arguments,
or all the TLEs of the files given as arguments, printing the propagated TEME
position and velocity to the standard output:

    sbt "reports/runMain sgp4.Main [options] [TLE line 1] [TLE line 2] [TLE files ...]"

Each argument that names an existing file is a TLE file, in the 2 line or the 3
line (with name) format; lines starting with `"1 "` and `"2 "` form an inline two
line element set. The options select the propagation model (`-v` Vallado, the
default, `-vl` Vallado-Lyddane long, `-p` polar nodals, `-l` Lara), the
propagation times (`-t 0,720,1440` in minutes from epoch, defaulting to one day,
every minute) and the Earth gravity model (`-g wgs72|wgs72old|wgs84`, defaulting
to `wgs72`, as in the python sgp4 library). `--help` prints a help message that shows the propagation model
being used. Deep space TLEs are reported and skipped: only the near Earth SGP4
algorithm is implemented.


Requires
---------------
* JDK 11 or newer. Both LTS JDKs 21 and 25 are verified: all tests pass on
  either (pass `JAVA_HOME` to sbt to pick one), and the propagation hot path
  keeps the same performance profile on both (see the benchmarks section).
* [sbt 1.x](https://www.scala-sbt.org) (the build uses sbt 1.13.0, declared in `project/build.properties`).
* The code runs on Scala 3.9.0, written with the Scala 3 idioms: significant
  indentation syntax, enums for the selection ADTs of the command line
  interface, `given`/`using` for the type class evidence, single clause
  packages and top level definitions in place of the package object. The
  migration went through a cross build on 2.13.18 and 3.9.0 (commit b00a178);
  on the propagation hot path Scala 3.9 stayed close to the Scala 2.13
  `@specialized` Double numbers, about 5-30% slower depending on the
  propagator (see the benchmarks section), and the generic spire Kepler solver
  still matches the hand written primitive Double loop. All dependencies
  (Spire 0.18.0, ScalaTest 3.2.20, better-files 3.9.2) are available for
  Scala 3.

Use
---------------
Clone and test the project

    git clone https://github.com/pleira/SGP4Extensions.git
    $ cd SGP4Extensions
    $ sbt
    > test

The test suite includes hardcoded reference values from Vallado's C++ code, and
two cross-checks of the propagation results against the
[python3-sgp4](https://pypi.org/project/sgp4/) library: one on the 2006 era
reference TLEs, and one on *current* TLEs downloaded from
[celestrak.org](https://celestrak.org). The reference data is regenerated with

    $ python3 scripts/gen-python-sgp4-reference.py     # 2006 era reference TLEs
    $ python3 scripts/gen-celestrak-reference.py        # current TLEs snapshot

which write `tests/src/test/resources/pythonSgp4Reference.csv` and
`celestrakReference.csv` (only rows for which the reference implementation
reports no error are included). On the current TLEs, SGP4Vallado agrees with
python3-sgp4 to about 1e-9 km; the periodic correction variants (ValladoLong,
PN, Lara) stay within their expected envelope of well under 1 km over a 6 day
propagation grid.

IDE
---------------
Any modern IDE imports this sbt project natively; no plugin-generated project
files are needed.

* IntelliJ IDEA: File > Open, point it at the repository root.
* VS Code with Metals: open the repository root (a `.metals` folder already exists).
* Eclipse: File > Import > Existing Projects into Workspace from the sbt import wizard.

Test
------------------
The best feature of SBT is incremental compilation. This is most evident when testing.

	> ~test

Be warned, it's addictive. You'll never want to code in any other statically compiled language without this feature.

Test a single TestSuite

	> testOnly package.subpackage.Class

Test and Jenkins
-------------------

ScalaTest is been configured to produce test reports in the JUnit XML format that Jenkins can understand in target/test-reports.
Configure Jenkins to use this folder. For many more options to configure, use the [ScalaTest runner documentation](http://www.scalatest.org/user_guide/using_the_runner)

Formatting
-------------------
Uses [scalafmt](https://github.com/scalameta/scalafmt); the configuration lives in `.scalafmt.conf`.

Format all sources:

	> scalafmtAll

Check them (e.g. in CI):

	> scalafmtCheckAll

Useful commands
-----------------
The former starts a Scala REPL with all the project dependencies available. The latter autoscans and runs a Main class (see the reports module for the available report programs, e.g. `reports/runMain sgp4.report.PropagateTLE28xxx`).
	> console
	> run

Benchmarks
-------------------
The `benchmarks` module contains [JMH](https://github.com/openjdk/jmh) micro-benchmarks
for the propagation hot path. It replaces the old `thymeBenchmarks` module, whose Thyme
dependency is no longer available. It measures:

* one full propagation of each SGP4 algorithm variant (Vallado, Vallado Long,
  Polar Nodals, Lara), with the propagator built once, and
* the cost of rebuilding the propagator on every call (`buildAndValladoPropagate`),
  which is the common misuse to avoid: build once, propagate many times,
* the generic spire Kepler equation solver against an equivalent hand-written
  primitive Double loop (they run at the same speed on Scala 3: the spire
  Double type class instances dispatch to primitive JVM signatures, and the
  measured generic solver is as fast as the loop, ~68 vs ~77 ns).

Run all benchmarks:

	> benchmarks/jmh:run

Run a subset, or include per-operation allocation with the JMH GC profiler:

	> benchmarks/jmh:run -prof gc .*ValladoPropagate.*
	> benchmarks/jmh:run -wi 5 -i 5 -f 1

Reference results on one machine, on Scala 3.9.0, side by side on the two
LTS JDKs (average time per propagation, `-wi 2 -i 3 -w 2 -r 2` reduced runs):

| Benchmark | JDK 21, TLE 00005 / 06251 | JDK 25, TLE 00005 / 06251 |
|---|---|---|
| valladoPropagate | ~402 / 380 ns | ~394 / 371 ns |
| valladoLongPropagate | ~423 / 392 ns | ~418 / 395 ns |
| polarNodalsPropagate | ~532 / 530 ns | ~521 / 536 ns |
| laraPropagate | ~578 / 556 ns | ~597 / 557 ns |
| buildAndValladoPropagate | ~1373 / 1279 ns | ~1358 / 1352 ns |

For reference, the same measurements on the Scala 2.13.18 cross build were
about 5-30% faster depending on the propagator (valladoPropagate ~381 / 357 ns
on JDK 25); that baseline lives in the git history of the migration.

Each valladoPropagate call allocates about 2.7 KB (`-prof gc`, ~2744 B/op on
JDK 25), the Or/case-class result chain of the pipeline. The generic spire
arithmetic is not the cost: its Double instances dispatch to
primitive-signature methods, which is what keeps the whole propagation
sub-microsecond and the allocation profile identical on both JDKs. If
throughput at scale ever becomes a problem, that allocation churn is where
to look.

For a description of the algorithm itself, see [sgp4.md](sgp4.md).

License
-------

See [LICENSE.txt](LICENSE.txt) for details.


