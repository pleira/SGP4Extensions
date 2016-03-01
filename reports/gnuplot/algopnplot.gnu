set datafile separator "|"

set xlabel "Minutes"
set ylabel "Difference"

set key left top
file = "algodiffpn.out"
# file = "5.out"

set terminal epslatex size 8.89cm,6.65cm color colortext
set output "dΩ_pn_00005.tex"
# set title "TLE 00005 SGP4 Algorithms Ω (Ascending Node) Difference in rad with SGP4Vallado"
set xlabel '$Minutes$'
set format '$%g$'

plot \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $7:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $7:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $7:1/0) with linespoints title "la"

set output "dI_pn_00005.tex"
# set title "TLE 00005 SGP4 Algorithms I (inclination)  Difference in rad with SGP4Vallado"

plot \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $5:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $5:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $5:1/0) with linespoints title "la"


set output "dθ_pn_00005.tex"
# set title "TLE 00005 SGP4 Algorithms θ (argument of latitude) Difference in rad with SGP4Vallado"

plot \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $6:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $6:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $6:1/0) with linespoints title "la"


set output "drθdot_pn_00005.tex"
# set title "TLE 00005 SGP4 Algorithms rθdot Difference  with SGP4Vallado"

plot \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $10:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $10:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $10:1/0) with linespoints title "la"

set output "dr_pn_00005.tex"
# set title "TLE 00005 SGP4 Algorithms r (radial) Difference in internal units with SGP4Vallado"

plot \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $8:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $8:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $8:1/0) with linespoints title "la"


set output "dRV_pn_00005.tex"
# set title "TLE 00005 SGP4 Algorithms Radial Velocity Difference in internal units with SGP4Vallado"

plot \
  file using 4:((stringcolumn(2) eq "vl" && ($3 == 5))? $9:1/0) with linespoints title "vl", \
  file using 4:((stringcolumn(2) eq "pn" && ($3 == 5))? $9:1/0) with linespoints title "pn", \
  file using 4:((stringcolumn(2) eq "la" && ($3 == 5))? $9:1/0) with linespoints title "la"
