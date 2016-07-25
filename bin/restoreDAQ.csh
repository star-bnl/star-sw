#!/bin/tcsh -f 
foreach f (`ls *.csh`) 
    set b = `basename $f .csh`
    set F = "/star/data03/daq/2005/*/$b.daq"
    echo $f "=>" $F
    if (! -r $F) echo "$F is missing"
end
#e o d
