#! /usr/local/bin/tcsh -f
#set list  = `find . -type l -name .rootrc`;
set list = `ls -l ./*/.[a-z]*/root/.rootrc | grep dev | awk '{print $9}'`
foreach root_rc ($list)
  set name = `basename $root_rc`
  set dir  = `dirname $root_rc` 
  echo $dir "->" $name
  cd $dir; rm $name; ln -s ../../ROOT/$name .
end
#last line
