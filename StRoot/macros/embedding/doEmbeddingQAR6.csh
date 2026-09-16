#!/bin/csh

if ( $#argv != 13 ) then
  echo ""
  echo " Usage : $0 [ignored] [year] [production] [input file list] [output prefix incl. dir.] [is embedding] [vzmax] [refmin] [refmax] [ptmax] [ptmin] [etamax] [ymax]"
  echo "    - ignored the fist argument is"
  echo "    - year - specifies the year of the data"
  echo "    - prouction - specifies the prodduction"
  echo "    - input file list - list of input minimc files"
  echo "    - output - basename of the output file, e.g. outdir/my.embedding.job.  Extension is determined by the next argument".
  echo "    - is embedding - set to kTRUE for embedding."
  echo "    - vzmax, refmin, refmax, etc... are kinematics cuts used in the embeddin job.  refmax is max multiplicity... use 1000."
  exit
endif

#$$$ starver $1
echo $1 specified, but ignored...

set year       = "$2"
set production = "$3"
set input      = "$4"
set output_prefix  = "$5"
set isembedding = "$6"
if ( "$isembedding" == "kTRUE" ) then
   set output = "${output_prefix}.embedding.root"
else
   set output = "${output_prefix}.real.root"
endif

set vzmax = "$7"
set refmin = "$8"
set refmax = "$9"
set ptmax = "$10"
set ptmin = "$11"
set etamax = "$12"
set ymax = "$13"

root4star -b <<EOF
  .L StRoot/macros/embedding/doEmbeddingQAMakerR6.C
  doEmbeddingQAMakerR6($year, "$production", "$input", "$output", $isembedding, $vzmax, $refmin, $refmax, $ptmax, $ptmin, $etamax, $ymax);
  .q
EOF

