To understand the **SGP4 (Simplified General Perturbations 4)** algorithm for near-Earth satellites ($P < 225\text{ min}$), we must bridge classical Newtonian orbital mechanics, analytical perturbation theory, atmospheric density modeling, and non-inertial reference systems.

---

### 1. From Newton to Perturbations

In an ideal two-body system, Newtonian gravitation yields a spherically symmetric central potential:

$$V_{0}(r) = -\frac{\mu}{r}$$

where $\mu = GM_{\oplus}$ is Earth's standard gravitational parameter. The resulting equation of motion, $\ddot{\mathbf{r}} = -\frac{\mu}{r^3}\mathbf{r}$, integrates analytically into a fixed Keplerian ellipse defined by six classical orbital elements: semi-major axis ($a$), eccentricity ($e$), inclination ($i$), right ascension of ascending node ($\Omega$), argument of perigee ($\omega$), and mean anomaly ($M$).

Real orbital motion deviates from this ideal due to disturbing accelerations $\mathbf{a}_{\text{pert}}$:

$$\ddot{\mathbf{r}} = -\frac{\mu}{r^3}\mathbf{r} + \mathbf{a}_{\text{pert}}$$

In numerical propagators (Special Perturbations), this differential equation is integrated numerically step-by-step. **SGP4 is a General Perturbations (GP) theory**: it integrates the equations of motion *analytically* using perturbation techniques (primarily Brouwer’s secular, long-period, and short-period averaging theories, combined with Lane’s atmospheric drag model).

---

### 2. Gravitational Model & Potential Coefficients

Earth is an oblate spheroid, flattened at the poles and bulging at the equator. The gravitational potential $V(r, \phi, \lambda)$ is modeled using spherical harmonics:

$$V(r, \phi) = -\frac{\mu}{r} \left[ 1 - \sum_{n=2}^{\infty} J_n \left( \frac{R_{\oplus}}{r} \right)^n P_n(\sin\phi) \right]$$

where $R_{\oplus}$ is Earth's equatorial radius, $\phi$ is geocentric latitude, $P_n$ are Legendre polynomials, and $J_n$ are zonal harmonic coefficients.

#### Specific $J_n$ Coefficients in SGP4

SGP4 retains terms up to $J_4$ using historical gravity models (typically **WGS-72** or **WGS-84**):

* **$J_2 \approx 1.08263 \times 10^{-3}$** (Earth oblateness): The dominant perturbation ($10^3\times$ larger than any other $J_n$).
* **$J_3 \approx -2.5388 \times 10^{-6}$** (Pear-shape asymmetry): Drives long-period oscillations in eccentricity and argument of perigee.
* **$J_4 \approx -1.6109 \times 10^{-6}$** (Polar flattening asymmetry).

#### The Kozai–Brouwer Mean Elements Transformation

Because SGP4 works analytically, the classical elements are separated into **secular variations** (linear time shifts), **long-period variations** (periodic with $\omega$), and **short-period variations** (periodic with the orbital period/mean anomaly $M$).

Using Delaunay canonical variables and von Zeipel transformations, $J_2$ induces secular drift rates on $\Omega$, $\omega$, and $M$:

$$\dot{\Omega}_{\text{sec}} = -\frac{3}{2} J_2 \left( \frac{R_{\oplus}}{p} \right)^2 n_0 \cos i_0$$

$$\dot{\omega}_{\text{sec}} = \frac{3}{4} J_2 \left( \frac{R_{\oplus}}{p} \right)^2 n_0 \left( 5\cos^2 i_0 - 1 \right)$$

$$\dot{M}_{\text{sec}} = n_0 \left[ 1 + \frac{3}{2} J_2 \left( \frac{R_{\oplus}}{p} \right)^2 \sqrt{1 - e_0^2} \left( \frac{3}{2} \cos^2 i_0 - \frac{1}{2} \right) \right]$$

where $p = a_0(1 - e_0^2)$ is the semi-latus rectum and $n_0 = \sqrt{\mu / a_0^3}$ is the mean motion.

---

### 3. Atmospheric Correction & Drag Model

At altitudes below $\sim 1000\text{ km}$, atmospheric drag exerts a continuous non-conservative force:

$$\mathbf{a}_{\text{drag}} = -\frac{1}{2} C_D \frac{A}{m} \rho v_{\text{rel}} \mathbf{v}_{\text{rel}}$$

where $C_D$ is the drag coefficient, $A/m$ is the area-to-mass ratio, and $\rho$ is local atmospheric density.

#### The $B^*$ Ballistic Coefficient

SGP4 folds object geometry, mass, drag coefficient, and reference atmospheric density $\rho_0$ into a single fitted parameter called **$B^*$**:

$$B^* = \frac{C_D A}{2m} \rho_0$$

In a Two-Line Element (TLE) set, $B^*$ is provided directly in units of $\text{Earth radii}^{-1}$.

#### Analytical Integration of Drag (Lane Theory)

SGP4 uses **Lane's exponential atmospheric density model**, where density scales with altitude $q_0$ above perigee:

$$\rho(h) = \rho_0 \exp\left( -\frac{h - h_0}{H} \right)$$

Lane integrated this non-linear drag acceleration over an eccentric orbit analytically by expanding atmospheric density in terms of power series near perigee.

SGP4 accounts for decaying semi-major axis $a(t)$ and eccentricity $e(t)$ over time using higher-order derivatives ($\dot{n}_0$, $\ddot{n}_0$) driven by $B^*$:

$$a(t) = a_0 \left[ 1 - C_1 t - C_2 t^2 \right]$$

$$e(t) = e_0 - \frac{B^* C_4}{a_0} t$$

*(where $C_1, C_2, C_4$ are internal SGP4 geometry-dependent drag coefficients calculated at epoch).*

---

### 4. Coordinates, Frames, and Transformations

To correctly propagate and output position and velocity, SGP4 operates across specific coordinate systems.

#### 1. True Equator, Mean Equinox (TEME) Frame

The direct spatial output of SGP4 is in the **TEME frame** (True Equator, Mean Equinox at Epoch).

* **Origin**: Geocentric.
* **$Z$-axis**: Direction of the true Earth rotational axis at epoch (includes nutation).
* **$X$-axis**: Direction toward the mean equinox of date (excludes short-term nutation effects).

> **Why TEME?** TEME is a quasi-inertial historical artifact. It avoids performing heavy precession/nutation rotations at every propagation step, keeping the analytical propagation computationally light.

#### 2. Osculating Position & Velocity Calculation

Once the updated mean elements $a(t), e(t), i(t), \Omega(t), \omega(t), M(t)$ are evaluated at target time $t$:

1. **Solve Kepler's Equation for Eccentric Anomaly $E$**:

$$M(t) = E - e(t) \sin E$$


2. **Calculate True Anomaly $\nu$ and Radius $r$**:

$$r = a(t) (1 - e(t) \cos E)$$


$$\tan\frac{\nu}{2} = \sqrt{\frac{1 + e(t)}{1 - e(t)}} \tan\frac{E}{2}$$


3. **Short-Period Re-injection**: Apply short-period $J_2$ corrections to $r, u = \omega + \nu, i, \Omega$ to transform mean osculating elements into true osculating elements ($r_k, u_k, i_k, \Omega_k$).
4. **Perifocal to TEME Vector Form**:

$$\mathbf{r}_{\text{TEME}} = r_k \begin{bmatrix} \cos u_k \cos \Omega_k - \sin u_k \sin \Omega_k \cos i_k \\ \cos u_k \sin \Omega_k + \sin u_k \cos \Omega_k \cos i_k \\ \sin u_k \sin i_k \end{bmatrix}$$



#### 3. Transforming TEME to Standard Inertial (GCRF/J2000) or Fixed (ITRF)

Because TEME is non-standard in modern astrodynamics, Dr. David Vallado (*Revisiting Spacetrack Report #3*, 2006) established standard transformations to convert TEME vectors to standard GCRF/ITRF:

$$\mathbf{r}_{\text{GCRF}} = \mathbf{P}(t) \cdot \mathbf{N}(t) \cdot \mathbf{R}_{\text{TEME}}(t) \cdot \mathbf{r}_{\text{TEME}}$$

where $\mathbf{R}_{\text{TEME}}$ applies the offset between mean equinox and true equinox (the equation of the equinoxes / nutation correction), $\mathbf{N}$ is the nutation matrix, and $\mathbf{P}$ is the precession matrix.

---

### 5. Summary of the SGP4 Workflow

To propagate a satellite from TLE inputs to Cartesian coordinates:

```
[TLE Inputs: i₀, Ω₀, e₀, ω₀, M₀, n₀, B*]
                   │
                   ▼
  1. Un-Kozai Recovery (Recover true original mean semi-major axis a₀)
                   │
                   ▼
  2. Compute Constants & Secular Drift Rates (J₂, J₃, J₄, drag coefficients C₁, C₄)
                   │
                   ▼
  3. Update Mean Elements at time t (secular J₂ drift + Lane drag decay)
                   │
                   ▼
  4. Solve Kepler's Eq (M(t) ──> E ──> ν)
                   │
                   ▼
  5. Add Short-Period J₂ Oscillations (Mean elements ──> Osculating elements)
                   │
                   ▼
  6. Compute Position & Velocity in TEME frame
                   │
                   ▼
  7. Optional Frame Transformation (TEME ──> GCRF / ITRF)

```
