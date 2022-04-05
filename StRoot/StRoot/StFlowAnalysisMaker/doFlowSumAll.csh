#!/bin/csh
# doFlowSumAll.csh
# runs doFlowSumAll.C for each centrality
# first cenNo is first arg
# first anaNo is second arg 
# dirName is third arg 
# $Id: doFlowSumAll.csh,v 1.2 2011/07/25 15:54:51 posk Exp $

if ($# != 3) then
    echo "usage:  $0 firstCenNo firstAnaNo dirName"
    exit 1
endif

set firstCenNo = $1
set firstAnaNo = $2
set dirName = $3
set cenN = 9

set cen = 0
while ($cenN - $cen)
    @ cenNo = $firstCenNo + $cen
    @ anaNo = $firstAnaNo + $cen
    echo "$dirName $cenNo to $anaNo"
root4star -b << FINIS
.x doFlowSumAll.C($cenNo,$cenNo,"$dirName",$anaNo)
.q
FINIS
    @ cen++
end

# $Log: doFlowSumAll.csh,v $
# Revision 1.2  2011/07/25 15:54:51  posk
# Added correction for non-flatness of event plane.
#
# Revision 1.1  2006/03/22 21:59:53  posk
# Macro and shell script to sum the outputs of the second pass.

