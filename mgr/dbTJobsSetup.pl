#!/opt/star/bin/perl -w
#
# 
#
# 
#
# L.Didenko
######################################################################
#
# dbTJobsSetup.pl
#

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="TestJobs";


# Tables
$FilesCatalogT = "FilesCatalog";
$JobTStatusT = "JobTStatus";

######################
sub StDbTJobsConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbTJobsDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

