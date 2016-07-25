#! /usr/local/bin/tcsh -f
set dlist="/afs/rhic/.asis/i386_redhat73/cern/2003/lib \
           /afs/rhic/.asis/i386_redhat73/cern/2002/lib \
           /afs/rhic/.asis/i386_redhat72/cern/2003/lib \
           /afs/rhic/.asis/i386_redhat72/cern/2002/lib \
           /afs/rhic/.asis/i386_redhat72/cern/2001/lib"
set libs = "libkernlib libpacklib"
foreach d ($dlist) 
  cd $d
  echo $d
  foreach l ($libs)
    set lib=${l}.a
    set nlib=${l}_noshift.a
    set slib=${l}-shift.a
    echo "$lib => $nlib"
    if (-l ${slib}) then
	echo rm $slib
    endif
    if (-f $lib && -f $nlib && -l ) then
	echo "<================"
	echo mv ${lib} ${l}-shift.a
	echo ln -sf ${l}_noshfit.a ${l}.a
    endif
  end
end
#e o f
