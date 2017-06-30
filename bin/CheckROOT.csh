#!/bin/tcsh -f 
foreach f ( `ls -1d $ROOTROOT/.sl*/*/lib` )
#    ls -ltr ${f}/*.so  | grep -v 'Jun  2' | tail -1
    ls -ltr ${f}/*.so  | tail -1
end
# E o D
