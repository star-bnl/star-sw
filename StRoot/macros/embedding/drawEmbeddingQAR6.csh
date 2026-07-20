#!/bin/csh

if ( $#argv != 13 ) then
  echo ""
  echo " Usage : $0 [starlib] [embedding root] [real root] [geantid] [ptmax] [embedonly] [parentgeantid] [vzmax] [refmin] [refmax] [ptmin] [etamax] [ymax]"
  echo ""
  echo "   starlib - is ignored"
  echo "   embedding root - root file from doEmbeddingQAR6.C  (is embedding = kTRUE)"
  echo "   real root - root file from doEmbeddingQAR6.C (is embedding = kFALSE)"
  echo "   geantid - the particle ID (geantid ... or possibly pdg id... of the generated particle.  we may have to update code for pdg ids... TBD.)"
  echo "   ptmax - the max pt for the plots"
  echo "   embedobly - kTRUE or kFALSE.  embed only =kTRUE shows only the embedded simulation.... use kFALSE."
  echo "   parentgeantid - ..."
  echo "   vzmax, etc... - kinematic variables... refmax is maximum reference multiplicty.  Use 1000."
  echo ""
  exit
endif

# starver $1

set embedroot = "$2"
set realroot = "$3"
set geantid = "$4"
set ptmax = "$5"
set embedonly = "$6"
set parentgeantid = "$7"

set vzmax = "$8"
set refmin = "$9"
set refmax = "$10"
set ptmin = "$11"
set etamax = "$12"
set ymax = "$13"

root4star -b <<EOF
  .L StRoot/macros/embedding/drawEmbeddingQAR6.C
  drawEmbeddingQAR6("./","$embedroot", "$realroot",$geantid,$ptmax,$embedonly,$parentgeantid,$vzmax, $refmin, $refmax, $ptmin, $etamax, $ymax);
  .q
EOF

