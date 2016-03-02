#!/opt/star/bin/perl -w
#
# $Id:
#
# $Log:
#
# L.Didenko
######################################################################
#
# dbCRSSetup.pl
#

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";


# Tables
$crsJobStatusT = "crsJobStatusY6";
$crsQueueT = "crsQueueY6";

######################
sub StcrsdbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StcrsdbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

