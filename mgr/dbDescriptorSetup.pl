#!/opt/star/bin/perl -w
#
# 
#
# 
#
# L.Didenko
######################################################################
#
# dbDescriptorSetup.pl
#

use DBI;

$dbhosto="onlsun1.star.bnl.gov:3306";
$dbusero="dbmole";
$dbpasso="quegp23";
$dbnameo="RunLog";
#$dbport="3316";


# Tables
$runDescriptorT = "runDescriptor";
$daqDescriptorT = "daqDescriptor";

######################
sub StDbDescriptorConnect {
    $dbh = DBI->connect("dbi:mysql:$dbnameo:$dbhosto", $dbusero, $dbpasso)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDescriptorDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

