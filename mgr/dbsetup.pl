#!/opt/star/bin/perl -w
#
# $Id: dbsetup.pl,v 1.1 1999/06/25 15:17:00 wenaus Exp $
#
# $Log: dbsetup.pl,v $
# Revision 1.1  1999/06/25 15:17:00  wenaus
# Add scripts for managing prod DB and SW guide
#
#
######################################################################
#
# dbsetup.pl
#
# T. Wenaus 5/99
#
# Production database setup routines
#

use DBI;

$dbuser="root";
$dbpass="octopus3";
$dbname="";
if ($dbuser eq 'root') {
    $dbname="system_data";
} else {
    $dbname=$dbuser."_data";
}

# Tables
$DataFileT = "DataFile";
$FileLocationT = "FileLocation";
$DataSetT = "DataSet";
$SubsetT = "Subset";
$DataDirT = "DataDir";
$RunT = "Run";
$EventT = "Event";
$RunFileT = "RunFile";

######################
sub StDbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

1;
