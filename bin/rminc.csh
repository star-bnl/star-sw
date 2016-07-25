#! /usr/local/bin/tcsh -f
set list = `ls *.idl`;
foreach idl ($list)
  set inc = $idl:r.inc
  set h   = $idl:r.h
  echo $idl "->" $inc $h
  rm $inc $h
end
#end_of script 
