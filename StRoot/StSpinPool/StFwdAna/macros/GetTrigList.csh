#!/bin/csh

# PURPOSE Handy script to generate a list of FCS triggers and their offline bit Ids for Run 22. Database access only possible in STAR online machines
#
# DESCRIPTION
# It will query the STAR database for all the offline bits in Run 22 related to the FCS. The #dbserver and #dbport is what signifies Run 22 and it uses mysql query which may not be possible from starsub machines which are usually not authenicated to do such queries
#
# LOG
# @[September 9, 2024] > Copied from https://www.star.bnl.gov/protected/spin/akio/fcs/trgid/offbit

set file=offlinebits.txt

#set dbserver=onldb2.starp.bnl.gov
#set dbport=3501
set dbserver=dbbak.starp.bnl.gov 
set dbport=3421

if ( -e $file ) then
   /bin/rm $file
endif

echo mysql -h $dbserver --port=$dbport
mysql --skip-column-names -h $dbserver --port=$dbport > $file << END1
connect Conditions_rts;
select name,offlineBit,min(idx_rn),max(idx_rn) from triggers where name like "fcs%" GROUP BY offlineBit ORDER BY name DESC;
END1

