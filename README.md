SGP4Extensions
------------

Scala project to try different SGP4 algorithm implementations. SGP4 is used to predict orbits from TLE data. The Deep Space algorithm, which contains lunar-solar perturbations, is not implemented. 

Most of the code is based in the work done by Vallado, documented in [SpaceTrackReport #3](https://celestrak.com/NORAD/documentation/). To verify the results, the cpp [public code](http://celestrak.com/publications/AIAA/2006-6753/) has been taken. Also, I have looked into the TLE processing done in the [Orekit](http://www.orekit.org) project.
My initial aim is to introduce an alternative implementation for the calculation of the perturbations suggested by [Martin Lara](http://arxiv.org/pdf/1407.8076.pdf). 

This implementation is also an exercise of having cleaner code written in Scala. That means, the physical models and transformations should appear clearer in the code. 

The numerical library Spire is used. In particular, the user can work with other numerical types that can offer more precision than Double. That can be of interest near conflictive points in the theory (low inclinations, and 63 deg) at the expense of longer running time. 

The SGP4 pipeline
====================

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

What Lara changes
--------------------

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
--------------------

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
* `reports`: command line programs that write comparison reports
* `benchmarks`: the JMH micro-benchmarks


Copyright and License
====================

This software is Copyright by Pablo Pita Leira, 2015. Licensed under Apache v. 2.0.

The software contains parts adapted from an SGP 4 s/w implementation in the public domain from Vallado. Some parts of the software were inspired by the SGP4 Orekit implementation, specially at the beginning of this project. Software taken from Orekit has copyright by CS Systèmes d'Information, licensed under Apache v. 2.0. 


Requires
---------------
* JDK 11 or newer (a current LTS JDK such as 21 is recommended).
* [sbt 1.x](https://www.scala-sbt.org) (the build uses sbt 1.13.0, declared in `project/build.properties`).
* The code runs on Scala 2.13.18 with Spire 0.18.0, ScalaTest 3.2.20 and better-files 3.9.2.

Use
---------------
Clone and test the project

    git clone https://github.com/pleira/SGP4Extensions.git
    $ cd SGP4Extensions
    $ sbt
    > test

The test suite includes hardcoded reference values from Vallado's C++ code, and
a cross-check of the propagation results against the [python3-sgp4](https://pypi.org/project/sgp4/)
library. The reference data for the latter is regenerated with

    $ python3 scripts/gen-python-sgp4-reference.py

which writes `tests/src/test/resources/pythonSgp4Reference.csv` (only rows for
which the reference implementation reports no error are included).

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
  primitive Double loop (they run at the same speed: the code is `@specialized`
  for Double and spire's Double type class instances use primitive JVM signatures).

Run all benchmarks:

	> benchmarks/jmh:run

Run a subset, or include per-operation allocation with the JMH GC profiler:

	> benchmarks/jmh:run -prof gc .*ValladoPropagate.*
	> benchmarks/jmh:run -wi 5 -i 5 -f 1

Reference results on one machine (JDK 21, average time per propagation):

| Benchmark | TLE 00005 | TLE 06251 |
|---|---|---|
| valladoPropagate | ~393 ns | ~369 ns |
| valladoLongPropagate | ~396 ns | ~377 ns |
| polarNodalsPropagate | ~459 ns | ~460 ns |
| laraPropagate | ~463 ns | ~499 ns |
| buildAndValladoPropagate | ~1142 ns | ~1128 ns |

Each propagation allocates about 2.3-3.3 KB (the Or/tuple/case-class result
chain; the generic spire arithmetic is not the cost, the code is @specialized
for Double and dispatches to primitive-signature methods). If throughput at
scale ever becomes a problem, that allocation churn is where to look.

For a description of the algorithm itself, see [sgp4.md](sgp4.md).

Author
--------------------
Pablo Pita

