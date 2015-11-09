An analysis of new implementations of the SGP4 algorithm in Scala is done.
These implementations have different characteristics regarding speed and applicability. As well, when processing the TLE object catalog to find out collision events, they provide good metadata that allow the integration of new algorithms for collision detection. 


### Rationale

This project initially was about testing new software techniques to develop solutions for scientific and engineering problems. Therefore, provides these characteristics:

* Scala based to use functional programing paradigm
* Avoidance of mutable state
* Parameterisation of numeric types in the algorithms
* unicode variables to help reading the software against ecuations given in the literature
* better structure to allow the introduction of new algorithms 
* choice of algorithms 
* fast through specialization/inlining

The improvements can provide more flexibility and more capabilities for masive data processing operations. Here is where this project, about orbit propagation, could set new goals which are related with operational problems.

### Goals 

* Be a good base for a Debris Collision Avoidance system. 
* Measure how fast this software can process and propagate orbits described by TLEs in the Java Virtual Machine. 
* Describe out trade-offs related to SGP4 algorithms for orbit propagation.
* Processing context calculated during orbit propagation is kept. That provides metadata for later on applying different kinds of filters and sieves when finding collision events. 
* Design the software so that it is easy and fast to introduce algorithms to find collision events.

Ideas

* Numeric types I: effects of introducing high precision numeric types. Slows down a lot, but are they useful in critical situations in certain algorithms?
* Numeric types II: where can numeric types like intervals (which would account for error margins) be used ?
* Numeric types III: is there a place for dual (Jet) numbers that provide "automatic derivation" in the algorithms?
* Software architecture: use of Abstract Syntax Trees to describe the operationsinstead of providing Object Oriented Interfaces to high level processes in the SGP4 algorithm in order to allow the user to choose which algorithm is used.
* Software architecture: create interpretes for side effects like outputing files. The software should be pure in the functional sense, side effects only in the higher software layers. 
* Units: use value objects for certain units, specially those related to time. There are days, minutes, julian days... Check if there are improvements regarding type safety for operating with that kind of data. 

### Physics

* relate software types with phyical variables in comparison with current SGP4 implementations.
* improve code self-description of the physics and equations used compared with other SGP4 implementations.
* As example, separate the derivation of the Potential Model coefficients which includes the atmospheric term.

