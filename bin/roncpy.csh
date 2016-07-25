#! /usr/bin/csh -f
set dirlist = "lisa longacre  broken mevsim nexus"
set host = `hostname | awk -F. '{print $1}'`
cd /home/scratch
set export = /star/rcf/data06/ron
set expDir = ${export}/${host}
if (! -d $expDir) mkdir $expDir
foreach dir ($dirlist) 
 if (-d $dir) then
    if (! -d $expDir/${dir}) mkdir $expDir/${dir}
    time mirdir -v -o -e none $dir $expDir/${dir} >& $expDir/${dir}.Log 
  endif
end
touch ${export}/${host}.Done
# last line
 
