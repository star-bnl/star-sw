#!/opt/star/bin/perl -w
#
# 
#
# 
#
#
######################################################################
#
# dbDstProdSetup.pl
#

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";
$SystemData="system_data";


# Tables
$DstProductionT = "DstProduction";

######################
sub StDbDstProdConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDstProdDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

