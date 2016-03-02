#!/usr/bin/perl 
#
# $Id: dbDuplicateAll.pl,v 1.2 2003/01/09 20:30:26 porter Exp $
#
# Author: R. Jeff Porter
#***************************************************************************
#
# Description: script to duplicate one or all databases from 
#              1 server to another
#
#****************************************************************************
# 
# $Log: dbDuplicateAll.pl,v $
# Revision 1.2  2003/01/09 20:30:26  porter
# upgrade of db table structure scripts
#
# Revision 1.1  2000/04/28 14:08:03  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use Getopt::Std;
use DBI;

getopts('i:o:d:fmhg');
$InputServer=$opt_i;
$OutputServer=$opt_o;
$databaseName=$opt_d;
$includeMysql=$opt_m;
$debug=$opt_g;

($opt_h or not defined $InputServer or not defined $OutputServer) and Usage();

my ($ihost, $iport, $ohost, $oport, $clihost, $clohost );

print $opt_i," ",$opt_o,"\n";
print $InputServer," ",$OutputServer,"\n";
if(!$InputServer or !$OutputServer){ Usage();}

if($InputServer=~/\:/){
    @tmp=split /\:/, $InputServer, 2;    
    $ihost=$tmp[0];
    $iport=$tmp[1];
    $clihost= qq{ -h $ihost --port=$iport -u nobody }
} else {
    $ihost=$InputServer;
    $iport=3306;
    $clihost= qq{ -h $ihost -u nobody }
}

if($OutputServer=~/\:/){
    @tmp=split /\:/, $OutputServer, 2;    
    $ohost=$tmp[0];
    $oport=$tmp[1];
    $clohost= qq{ -h $ohost --port=$oport }
} else {
    $ohost=$OutputServer;
    $oport=3306;
    $clohost= qq{ -h $ohost }
}

my @databaseNames='';
if(!$databaseName){
    $#databaseNames=0;
    $databaseNames[0]="AnyDataBase";
} else {
  @databaseNames=split(/:/,$databaseName);
}


#---- find databases ------------------------------
        $drh = DBI->install_driver("mysql");
        @idatabases = $drh->func($ihost, $iport, '_ListDBs');
#        @odatabases = $drh->func($ohost, $oport, '_ListDBs');

#---- loop over most databases (skip 'mysql') -----

$mbindir="/usr/bin";



for($j=0;$j<=$#databaseNames;$j++){
  for($i=0;$i<=$#idatabases;$i++){
    if(!($databaseNames[$j]=~m/AnyDataBase/)){
        if($idatabases[$i] != $databaseNames[$j]){ next; }
    } else {
    if(!($includeMysql) && $idatabases[$i]=~m/mysql/){ next;}
    if($idatabases[$i]=~m/test/){ next; }
    if($idatabases[$i]=~m/Test/){ next; }
    

    $dbname=$idatabases[$i];
    $fname="/tmp/".$dbname.".sql";
    $cmd = "$mbindir/mysqldump --opt $clihost $dbname > $fname";
    print $cmd,"\n";
    if(!$debug){ $out=`$cmd`;}

    $cmd = "$mbindir/mysql $clohost --execute='drop database if exists $dbname; create database $dbname'";
    print $cmd,"\n";
    if(!$debug){ $out=`$cmd`;}
    $cmd = "cat $fname | $mbindir/mysql $clohost -C $dbname";
    print $cmd,"\n";
    if(!$debug){ $out=`$cmd`;}
    $cmd = "rm $fname";
    print $cmd,"\n";
    if(!$debug){ $out=`$cmd`; }
  }

}
}

#-------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------
sub Usage() {

    print "\n";
print "****  Usage   :: dbDuplicateAll.pl  -i Inputserver -o Outputserver [-d database] [-m] [-h]\n";
print "****  Purpose :: script to duplicate dbs from 1 server to another\n";
    print "                 -i/o server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
    print "                 -d database for a selected list of databases to be duplicated\n";
    print "                 list format = db1:db2:db3:...:dbn\n";
    print "                 -m  flag to include the 'mysql' database \n";
    print "                 -h  print this\n\n";
print "****  Requires  **** Write-Access to all dbs on output server\n\n";

exit;

}



