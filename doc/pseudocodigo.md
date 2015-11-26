### pseudo codigo Vallado

Primero, se crea el propagador SGP4 relacionado con un TLE.
* TLE se forma a partir de las lineas 1 y 2
* Se obtiene la primera estructura SGPElem a partir de ese TLE, sin transformar nada
* Se hace la transformación Double Primes, es decir, se recupera el n0 (mean motion) y el a0 (semimajor axis) originales
* Se guarda en una estructura DpState (que contiene SGPElem con los elementos double prime, mas un primer Context con variables generadas de la inclinacion i0 y la eccentricidad e0)
* Vamos a ir a por unos términos relacionados con la atmósfera con el DpState. Se calcula el s, fitting atmospheric parameter, a partir de la altura del perigeo. 
* Vamos a por el GeoPotentialState que describe el geo potencial, es decir, calcula los coeficientes del potencial C1...D4 a partir del s y lo que hay en el DpState
* Con el GeoPotentialState estamos en condiciones de inicializar una instancia del SecularEffects.
* La instancia de secular effects es la base para calcular los efectos como funcion del tiempo t. El resultado se guarda en SecularState

Con el SecularState se inicializa el propagador que hará los cálculos de las propagaciones a diversos tiempos. Estas propagaciones no necesitan volver a calcular pasos ya dados. 
Luego viene:
* Se calcula a tiempo t los efectos seculares en variables de Delauney. 
* Se calculan los LongPeriodPeriodicEffects con las variables de Lyddane que dan lugar al LongPeriodPeriodicState
* Se resuelve la ecuacion de Kepler con KeplerSolver (algoritmo Newton Raphson) que da lugar al EccentricAnomalyState basada en Lyddane.
* Para resolver los efectos de corto periodo, se pasa de Lyddane a variables Polares Nodales. 
* Las correciones de corto periodo se hacen en variables polares nodales. Así los ShortPeriodPeriodicEffects dan lugar al ShortPeriodPeriodicState en polares nodales.
* Se obtenie así el estado final de la órbita en tiempo t en variables polares nodales
* Se hace una transformación a las variables cartesianas de posición y velocidad en TEME

### pseudo codigo Lara

La implementación de Lara es igual que la de Vallado hasta el calculo a tiempo t de los efectos seculares en variables de Delauney.
A partir de ahí se hace lo siguiente:
* En primer lugar se calcula aquí la ecuación de Kepler (sin que se hayan hecho las correcciones de largo periodo). 
* Se obtiene la Eccentric Anomaly y la true Anomaly en variables de Delauney. 
* Luego hay que ir de Delauney a lass nuevas variables. Eso requiere pasar de Delauney a PolaresNodales, y de Polares Nodales, a las variables no singulares de Lara.
* Ahí estamos en condiciones de aplicar las correcciones de largo periodo y de corto periodo.
* Luego se convierten las variables singulares a polares nodales
* Con ello obtenemos el estado final de la órbita en tiempo t en polares nodales
* las variables polares nodales se convierten en cartesianas.

### observaciones:

* Lara resuelve la ecuación de Kepler antes de aplicar las correcciones de largo periodo
* Se podrían usar las variables polares podales para comparar los resultados de la implementación de Lara con los de Vallado. 
* No hace falta convertir a variables cartesianas, aunque se puede.
* Habrá que examinar los valores intermedios de las correcciones para validar el algoritmo 
* Sería interesante extraer las correcciones de largo periodo y de corto periodo que obtendría Vallado (en polares nodales) y las dadas por Lara para compararlas

