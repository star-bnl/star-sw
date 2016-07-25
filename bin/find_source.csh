#!/bin/tcsh -f
set list = `find . -type d`;
foreach d ($list) 
#    echo $d
    if ($d != "CVS") then
	grep CERNLIB_QMLXIA64 $d/*.*
    endif
end
# e o d
