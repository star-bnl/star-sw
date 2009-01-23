#!/usr/bin/perl -w

###########################################################
use strict;

###############################################################
# Get list of uDST files here
chomp(my @proc_list = `ps  -u panitkin |grep -i make`);

print "@proc_list \n"; 

#kill 0 => $proc_number
exit;
