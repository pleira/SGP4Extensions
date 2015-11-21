### pseudo codigo 

* TLE se forma a partir de las lineas 1 y 2
* Se obtiene la primera estructura SGPElem a partir de ese TLE, sin transformar nada
* Se hace la transformación Double Primes, es decir, se recupera el n0 (mean motion) y el a0 (semimajor axis) originales . Se guarda en una estructura DpState (que contiene SGPElem con los elementos double prime, mas un primer Context con variables generadas de la inclinacion i0 y la eccentricidad e0)
* Vamos a ir a por unos términos relacionados con la atmósfera con el DpState. Se calcula el s, fitting atmospheric parameter, a partir de la altura del perigeo. 
* Vamos a por el GeoPotentialState que describe el geo potencial, es decir, calcula los coeficientes del potencial C1...D4 a partir del s y lo que hay en el DpState
* Con el GeoPotentialState estamos en condiciones de inicializar una instancia del SecularEffects.

Hasta aqui, todo esta calculado sin depender del tiempo. Es importante tenerlo separado, pues las propagaciones a diversos tiempos no necesitan volver a calcular pasos ya dados. Luego viene :

* La instancia de secular effects es la base para calcular los efectos como funcion del tiempo t. El resultado se guarda en SecularState  
* Luego vienen los LongPeriodPeriodicEffects que dan lugar al  LongPeriodPeriodicState
* Se resuelve la ecuacion de Kepler con KeplerSolver (algoritmo Newton Raphson) que da lugar al EccentricAnomalyState
* Luego vienen los ShortPeriodPeriodicEffects que da lugar al  ShortPeriodPeriodicState
* Se hace una transformación a las variables cartesianas de posición y velocidad en TEME
 
