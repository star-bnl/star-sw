#!/opt/star/bin/perl -w
#
# 
#
# 
#
#
######################################################################
#
# dbDaqOperaSetup.pl
#

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";
$SystemData="system_data";


# Tables
$DaqOperationT = "DaqOperation";

######################
sub StDbDaqOperaConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDaqOperaDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

