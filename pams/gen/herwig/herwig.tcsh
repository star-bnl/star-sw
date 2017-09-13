#!/usr/local/bin/tcsh -f
# Add automatic seeds via sampling the time and process ID:
cat <<EOF > herwig.in
11111       run number
10          number of events
P           proton
P           proton
100.0       beam energy
1500        QCD is 1500
7.0         pt max
5.0         pt min
`date +%S`
$$
EOF
herwig_tupl.x
