set datafile separator "|"

set xlabel "Minutes"
set ylabel "Difference"

set key left top
file = "diffcartesians.out"
# file = "5.out"

set terminal png size 800, 600
set output "dz_cartesian_00005.png"
set title "TLE 00005 SGP4 Algorithms Z Difference in km with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $7:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $7:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $7:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $7:1/0) with linespoints title "la"

set output "dx_cartesian_00005.png"
set title "TLE 00005 SGP4 Algorithms X Difference in km with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $5:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $5:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $5:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $5:1/0) with linespoints title "la"


set output "dy_cartesian_00005.png"
set title "TLE 00005 SGP4 Algorithms Y Difference in km with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $6:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $6:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $6:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $6:1/0) with linespoints title "la"


set output "dvz_cartesian_00005.png"
set title "TLE 00005 SGP4 Algorithms VZ Difference in km/s with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $10:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $10:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $10:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $10:1/0) with linespoints title "la"

set output "dvx_cartesian_00005.png"
set title "TLE 00005 SGP4 Algorithms VX Difference in km/s with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $8:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $8:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $8:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $8:1/0) with linespoints title "la"


set output "dvy_cartesian_00005.png"
set title "TLE 00005 SGP4 Algorithms VY Difference in km/s with Vallado's C++ Results"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $9:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $9:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $9:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $9:1/0) with linespoints title "la"


