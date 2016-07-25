#! /usr//bin/env tcsh
foreach f (`ls *.BAK`) 
    set s = `basename $f .BAK`
    echo "move " $f "=>" $s
    mv $f $s
end
# eod 
