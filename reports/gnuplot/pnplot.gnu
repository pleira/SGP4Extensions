set datafile separator "|"

set xlabel "Minutes"
set ylabel "Difference"

set key left top
file = "diffpn.out"
# file = "5.out"

set terminal png size 800, 600
#set terminal jpeg size 600, 400
set output "dΩ_pn_00005.png"
set title "TLE 00005 SGP4 Algorithms Ω (Ascending Node) Difference in rad with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $7:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $7:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $7:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $7:1/0) with linespoints title "la"

set output "dI_pn_00005.png"
set title "TLE 00005 SGP4 Algorithms I (inclination)  Difference in rad with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $5:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $5:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $5:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $5:1/0) with linespoints title "la"


set output "dθ_pn_00005.png"
set title "TLE 00005 SGP4 Algorithms θ (argument of latitude) Difference in rad with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $6:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $6:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $6:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $6:1/0) with linespoints title "la"


set output "drθdot_pn_00005.png"
set title "TLE 00005 SGP4 Algorithms rθdot Difference  with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $10:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $10:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $10:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $10:1/0) with linespoints title "la"

set output "dr_pn_00005.png"
set title "TLE 00005 SGP4 Algorithms r (radial) Difference in internal units with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $8:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $8:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $8:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $8:1/0) with linespoints title "la"


set output "dRV_pn_00005.png"
set title "TLE 00005 SGP4 Algorithms Radial Velocity Difference in internal units with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $9:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $9:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $9:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $9:1/0) with linespoints title "la"


