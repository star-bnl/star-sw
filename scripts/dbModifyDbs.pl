#!/usr/bin/perl 
#
# $Id: dbModifyDbs.pl,v 1.2 2003/01/09 20:30:27 porter Exp $
#
# Author: R. Jeff Porter
#***************************************************************************
#
# Description: Example script to modify all databases in the same way
#
#
#****************************************************************************
# 
# $Log: dbModifyDbs.pl,v $
# Revision 1.2  2003/01/09 20:30:27  porter
# upgrade of db table structure scripts
#
# Revision 1.1  2000/04/28 14:08:04  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use Getopt::Std;
use DBI;

getopts('s:');
$serverHost=$opt_s;

if($serverHost=~/\:/){
    @tmp=split /\:/, $serverHost, 2;    
    $host=$tmp[0];
    $port=$tmp[1];
} else {
    $host=$serverHost;
    $port=3306;
}

my $dbh;

#---- find databases ------------------------------
#--> "nobody" is just a reader

        $dbh = DBI->connect("DBI:mysql:StarDb:$ihost:$iport","nobody","");
        @idatabases = $dbh->func('_ListDBs');
$dbh->disconnect;
#
#--> this will also work if user running script is allowed access without passwd
#
#        $drh = DBI->install_driver("mysql");
#        @databases = $drh->func($host, $port, '_ListDBs');
#
#---- loop over most databases (skip 'test%, 'mysql', & 'operations') -----

for($i=0;$i<=$#databases;$i++){
    if($databases[$i]=~m/test/ or $databases[$i]=~m/mysql/){ next;}
    if($databases[$i]=~m/operat/){ next;}

   $dbh = DBI->connect("DBI:mysql:$databases[$i]:$serverHost",$dbuser,$dbpass)
    || die "Cannot connect to server $DBI::errstr\n";

#------- find tables ----------------
    @tables = $dbh->func( '_ListTables' );

#------- find the table in question (here dataIndex) ------------------------

    $dataID=0;
    for($j=0;$j<=$#tables;$j++){
        if($tables[$j]=~m/dataIndex/){
            $dataID=1;
            print " Modifying dataIndex in DB=", $databases[$i],"\n";
        }
    }
#--------- here's a modify SQL that is commented out in this example ----
#    if($dataID){
#        $qmod=qq{alter table dataIndex} .
#              qq{ modify column dataID int DEFAULT '0' NOT NULL};
#        $qindex=qq{alter table dataIndex add index (dataID)};
#        $dbh->do($qmod);
#        $dbh->do($qindex);
#    }
#------------------------------------------------------------------------

#---- now disconnect ------------

    $dbh->disconnect;
}

#-------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------
sub Usage() {

    print "\n";
print "****  Usage   :: dbModifyDbs.pl  -s server\n";
print "****  Purpose :: example script to modify all dbs in the same way\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
    print "                Note this example does nothing\n";
print "****  Requires  **** Write-Access to all databases when it really does something\n";


exit;

}











