#! /usr/local/bin/tcsh -f
set list = `ls ../../cint/*.h ../../cint/src/*.*`
foreach file ($list)
  set name = `basename $file `
  set myname = ${name};
  if ( ! -f $name ) set name = CINT_$name
  if (-f $name) then
  echo "rm $myname; ln -s  ../../root/Cint/$name $myname"
#     diff -wb $file $name
#     cp $file $name
  endif
end
#last line
