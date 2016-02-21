set datafile separator "|"

set key left top
file = "diffcartesians.out"
# file = "5.out"

set terminal epslatex size 8.89cm,6.65cm color colortext
set output "dz_cartesian_00005.tex"

# set title "Z difference (km) of SGP4Extension algorithms with C++ results for TLE 00005"
set xlabel '$Minutes$'
set ylabel '$Difference (km)$'
set format '$%g$'

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $7:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $7:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $7:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $7:1/0) with linespoints title "la"

set output "dx_cartesian_00005.tex"
# set title "X difference (km) of SGP4Extension algorithms with C++ results for TLE 00005"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $5:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $5:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $5:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $5:1/0) with linespoints title "la"


set output "dy_cartesian_00005.tex"
# # set title "Y difference (km) of SGP4Extension algorithms with C++ results for TLE 00005"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $6:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $6:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $6:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $6:1/0) with linespoints title "la"

set xlabel '$Minutes$'
set ylabel '$Difference (km/s)$'
set format '$%g$'

set output "dvz_cartesian_00005.tex"
# set title "V_z difference (km/s) of SGP4Extension algorithms with C++ results for TLE 00005"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $10:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $10:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $10:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $10:1/0) with linespoints title "la"

set output "dvx_cartesian_00005.tex"
# set title "V_x difference (km/s) of SGP4Extension algorithms with C++ results for TLE 00005"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $8:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $8:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $8:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $8:1/0) with linespoints title "la"


set output "dvy_cartesian_00005.tex"
# set title "V_y difference (km/s) of SGP4Extension algorithms with C++ results for TLE 00005"

plot \
  file using 4:((stringcolumn(2) eq "va" && ($3 == 5))? $9:1/0) with linespoints title "va", \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $9:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $9:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $9:1/0) with linespoints title "la"


