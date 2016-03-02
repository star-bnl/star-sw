#!/usr/bin/env perl

use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use CRSQueues;

$PAT = "$LIB"."_*_st_*";


$TOT = CRSQ_getcnt(5,4,$PAT);

print "Got $TOT\n";
    
#CRSQ_check($PAT,"../archive");
