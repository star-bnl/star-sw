#!/bin/tcsh -f
foreach f (`ls  $ROOTROOT/geant3/minicern/*.F`) 
    set b = `basename $f`
    if (-r $b) then
	echo "Duplicated $f and $b"
	rm $b
    endif
end
# e o d
