#! /usr/local/bin/tcsh -f
set list  = "/afs/rhic/.asis/*/cern/*/lib/libpythia*.a";
foreach file ($list)
    echo $file
    set dir = `dirname $file `
    set name = `basename $file `
    cd $dir; rm libpythia.a; ln -s $name libpythia.a;
end 


