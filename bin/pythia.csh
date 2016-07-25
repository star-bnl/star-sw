#! /bin/csh -f
echo "make list"
set list = `ls /afs/rhic/.asis/*/cern/*/lib/libpythia6136.a`
#echo $list
foreach f ( $list )
    echo $f
  set dir = `dirname $f ` 
  set name = `basename $f `
  echo $dir "->" $name
  cd $dir
  ln -s $name libpythia.a
end
#end script
