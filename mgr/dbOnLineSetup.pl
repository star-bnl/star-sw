#!/opt/star/bin/perl -w
#
# 
#
# 
#
# L.Didenko
######################################################################
#
# dbOnLineSetup.pl
#

use DBI;

$dbhosto="onlsun1.star.bnl.gov:3316";
$dbusero="dbmole";
$dbpasso="quegp23";
$dbnameo="RunLog_daq";
#$dbport="3316";


# Tables
$daqTagFilesT = "daqTagFiles";
$daqRunTagT = "daqRunTag";

######################
sub StDbOnLineConnect {
    $dbh = DBI->connect("dbi:mysql:$dbnameo:$dbhosto", $dbusero, $dbpasso)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbOnLineDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

