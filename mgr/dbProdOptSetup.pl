#!/opt/star/bin/perl -w
#
# 
#
# 
#
# L.Didenko
######################################################################
#
# dbProdOptSetup.pl
#

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";
$SystemData="system_data";


# Tables
$ProdOptionsT = "ProdOptions";

######################
sub StDbProdOptionsConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbProdOptionsDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

