grep INPUTFILE0 `condor_q -l | grep Cmd | grep fisyak | awk -F\" '{print $2}'` | grep \.root
