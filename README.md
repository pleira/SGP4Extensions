SGP4Extensions
------------

Scala project to try different SGP4 algorithm implementations. SGP4 is used to predict orbits from TLE data. The Deep Space algorithm, which contains lunar-solar perturbations, is not implemented. 

Most of the code is based in the work done by Vallado, documented in [SpaceTrackReport #3](https://celestrak.com/NORAD/documentation/). To verify the results, the cpp [public code](http://celestrak.com/publications/AIAA/2006-6753/) has been taken. Also, I have looked into the TLE processing done in the [Orekit](http://www.orekit.org) project.
My initial aim is to introduce an alternative implementation for the calculation of the perturbations suggested by [Martin Lara](http://arxiv.org/pdf/1407.8076.pdf). 

This implementation is also an exercise of having cleaner code written in Scala. That means, the physical models and transformations should appear clearer in the code. 

The numerical library Spire is used. In particular, the user can work with other numerical types that can offer more precision than Double. That can be of interest near conflictive points in the theory (low inclinations, and 63 deg) at the expense of longer running time. 


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

Useful commands
-----------------
The former will run a scala REPL with all the project dependencies available. The later will autoscan and run a Main class (see the reports module for the available report programs, e.g. `reports/runMain predict4s.report.PropagateTLE28xxx`).
	> console
	> run

Author
--------------------
Pablo Pita

