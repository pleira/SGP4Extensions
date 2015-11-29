## SGP4 algorithm implementation using Lara's non singular variable set in a functional paradigm 

The SGP4 (Simplified General Perturbations 4) algorithm is a widely used orbital propagator. Current implementations are based on Brouwer's gravity solution using Lyddane's variable set. Lara has recently proposed an alternative nonsingular variable set for the calculation of the periodic terms in SGP4. This work presents new implementations of the SGP4 algorithm in Scala, one using Lara's new variable set and another one derived from the well-known Vallado's implementation as presented in his Spacetrack Revision #3 report. Scala is a hybrid functional/object oriented programming language running in the Java Virtual Machine that allows for functional features in the implementations. Validation of Lara's calculation for different orbits is performed using Vallado's results as reference. Finally, applicability for massive data processing tasks like prediction of orbital collision events and performance are discussed.


### Rationale

This project initially was about testing new software techniques to develop solutions for scientific and engineering problems. Therefore, provides these characteristics:

* Scala based to use functional programming paradigm
* Interpreter pattern in its core
* Avoidance of mutable state
* Parameterisation of numeric types in the algorithms
* unicode variables to help reading the software against equations given in the literature
* better structure to allow the introduction of new algorithms 
* choice of algorithms presented through Abstract Data Types in contrast to Object Oriented Interfaces 

The interpreter pattern design provides more flexibility and more capabilities for massive data processing operations. There is a separation of the run time part that has state, like reading/writing to files, from a pure functional part, which just is responsible to describe the algorithm applied. 

For processing the TLE object catalog to find out possible collision events, metadata produced during propagation allows the integration of new algorithms for collision detection. Abstract Data Types hide the different SGP4 implementations which provide different kinds of metadata. It is therefore, easier to integrate other analysis techniques with the generated orbital data from a massive catalog. Here is where this project about orbit propagation could set new goals which are related with operational problems.

### Goals 

* Be a good base for a Debris Collision Avoidance system. 
* Measure how fast this software can process and propagate orbits described by TLEs in the Java Virtual Machine. 
* Describe out trade-offs related to SGP4 algorithms for orbit propagation.
* Processing context calculated during orbit propagation is kept. That provides metadata for later on applying different kinds of filters and sieves when finding collision events. 
* Design the software so that it is easy and fast to introduce algorithms to find collision events.

### Ideas

* Numeric types I: effects of introducing high precision numeric types. Slows down a lot, but are they useful in critical situations in certain algorithms?
* Numeric types II: where can numeric types like intervals (which would account for error margins) be used ?
* Numeric types III: is there a place for dual (Jet) numbers that provide "automatic derivation" in the algorithms?
* Software architecture: use of Abstract Syntax Trees to describe the operations instead of providing Object Oriented Interfaces to high level processes in the SGP4 algorithm in order to allow the user to choose which algorithm is used.
* Software architecture: create interpretes for side effects like outputting files. The software should be pure in the functional sense, side effects only in the higher software layers. 
* Units: use value objects for certain units, specially those related to time. There are days, minutes, julian days... Check if there are improvements regarding type safety for operating with that kind of data. 

### Physics

* relate software types with physical variables in comparison with current SGP4 implementations.
* improve code self-description of the physics and equations used compared with other SGP4 implementations.
* As example, separate the derivation of the Potential Model coefficients which includes the atmospheric term.

### References

TBD

