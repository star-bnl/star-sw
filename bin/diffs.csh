#! /usr/local/bin/tcsh -f
set list = `ls $1.cc`
foreach file ($list)
  set f = `basename $file .cc`.cxx
  diff -wb $file $f
end
set list = `ls $1.hh`
foreach file ($list)
  set f = `basename $file .hh`.h
  diff -wb $file $f
end
#end_of_script
