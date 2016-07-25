#!/usr/bin/env tcsh 
foreach f ( `ls ../*.h` )
    ../../split.pl $f
end
# e o d
