## An implementation of SGP4 in non-singular variables using a functional paradigm 

The SGP4 (Simplified General Perturbations 4) orbit propagator is a widely used tool for the fast, short term propagation of space orbits. The algorithms in which it is based are thoroughly described in the SPACETRACK report #3, as well as in Vallado et al. update. Current implementations of SGP4 are based on Brouwer's gravity solution and Lane atmospheric model, but using Lyddane's modifications for avoiding loss of precision in the evaluation of the periodic corrections, which are, besides, notably simplified for improving evaluation efficiency. Different alternatives in the literature discuss other variable sets, either canonical or not, that can be used in the computation of periodic corrections (see Izsak, Aksnes, Hoots, or Lara).

This work presents a new implementation of the SGP4 algorithm in Scala that offers a choice about the variable set used for the computation of the periodic corrections. Scala is a hybrid functional/object oriented programming language running in the Java Virtual Machine that allows for incorporating functional features in the design. Validation of the new implementations is made by carrying out different tests based on Vallado's results. Finally, applicability for massive data processing tasks like prediction of orbital collision events and performance are discussed.


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

1.- Hoots, FR, Roehrich, RL, Models for Propagation of the NORAD Element Sets (SPACETRACK rep. #3), 1980.
2.- Vallado, D, Crawford, P, Hujsak, R, Kelso, TS, Revisiting Spacetrack Report #3, AIAA paper 2006-6753, 2006
3.- Brouwer, D, Solution of the problem of artificial satellite theory without drag, Astronomical Journal, vol. 64, pp. 378-397, 1959
4.- Lane, MH, The development of an artificial satellite theory using a power-law atmospheric density representation, AIAA paper 65-35, 1965
5.- Lyddane, RH, Small eccentricities or inclinations in the Brouwer theory of the artificial satellite, Astronomical Journal, vol. 68, pp. 555-558, 1963
6.- Izsak, IG, On satellite orbits with very small eccentricities, Astronomical Journal, vol. 66 pp. 129-131, 1961
7.- Aksnes, K, On the Use of the Hill Variables in Artificial Satellite Theory, Astronomy and Astrophysics, vol. 17, pp. 70-75, 1972
8.- Hoots, FR, Reformulation of the Brouwer geopotential theory for improved computational efficiency, Celestial Mechanics, vol. 24, pp. 367-375, 1981
9.- Lara, M, Efficient Formulation of the Periodic Corrections in Brouwer’s Gravity Solution, Mathematical Problems in Engineering, vol. 2015, Article ID 980652, 9 pp., 2015BD

