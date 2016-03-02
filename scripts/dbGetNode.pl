#!/usr/bin/perl 
#
#
# $Id: dbGetNode.pl,v 1.5 2007/05/04 03:54:53 deph Exp $
#
# Author: Bum Choi & R. Jeff Porter
#
#***************************************************************************
#
# Description: parses nodes from mysql into an xml format
#
#****************************************************************************
# 
# $Log: dbGetNode.pl,v $
# Revision 1.5  2007/05/04 03:54:53  deph
# New table with bit masks for pmdHotCells
#
# Revision 1.4  2003/01/09 20:30:27  porter
# upgrade of db table structure scripts
#
# Revision 1.2  2000/08/15 18:36:01  porter
# changed StDbtable to StDbTable
#
# Revision 1.1  2000/04/28 14:08:04  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################
# 
#===================================================================
# package ; (maybe later)
#===================================================================
use DBI;
#use strict;
use Getopt::Std;
use FileHandle;
#===================================================================
# -n nodeName -d databasename -s server -h 
getopts('hd:s:n:');
my $dbName     = $opt_d;
#my $outputfile = $opt_n; 
my $serverHost = $opt_s;
my $nodeName   = $opt_n;


($opt_h or not defined $dbName or not defined $serverHost) and Usage();
#-------------------------------------------------------------------
my ($dbh, $sth);  
my ($query, $elementid, $type);
my ($configname, $configversion, $configtype);
my ($name, $version, $nodetype, $structname, $baseline, $isbinary, $isindexed);
my $xml_fh;  # handle used in all the print statements
#-------------------------------------------------------------------
$dbh = DBI->connect("DBI:mysql:$dbName:$serverHost",'deph','')
#		       {RaiseError => 1, AutoCommit => 0})
  or die "couldnt connect to $dbName: $dbh->errstr\n";
#------------------------------------------------------------------- 
# get id of config node that is in NodeRelation
# for now, dont worry about a particular version

$query = qq{select distinct n.name, n.nodetype, n.versionkey } .
         qq{from Nodes as n, NodeRelation as r  } . 
         qq{where n.id = r.parentid and n.nodetype = 'Config'};

$sth=$dbh->prepare(qq{select name, versionkey, structname, } .
		   qq{baseline, isbinary, isindexed, elementid }.
		   qq{from Nodes where nodeType = ?});

($configname, $configtype,$configversion) = $dbh->selectrow_array($query);
#------------------------------------------------------------------
OpenFile($dbName);
HeaderStart();
MarkerStart($configname,"dbNode");
MarkerBoth($configtype,"type");
MarkerBoth($configversion,"version");
MarkerEnd("dbNode"); 

foreach $type  ('DB', 'table', 'directory'){
    $sth->execute($type);
    while(($name, $version, $structname, $baseline, $isbinary, $isindexed,
       $elementid) = $sth->fetchrow){
	MarkerStart($name,"dbNode");
	MarkerBoth($type,"type") if !($type eq 'table');
	MarkerBoth($version,"version") if !($version eq 'default');
	MarkerBoth($structname,"StDbTable") if $type eq 'table';
	MarkerBoth($baseline,"baseLine") if ($type eq 'table' 
	    and $baseline =~/Y/);
        MarkerBoth($isbinary,"isBinary") if ($type eq 'table' 
	    and $isbinary =~ /Y/);
        MarkerBoth($isindexed,"isIndexed") if ($type eq 'table' 
	    and $isindexed =~ /N/);
	MarkerBoth($elementid,"elementID") if !($elementid=~m/None/);
	MarkerEnd("dbNode");
    }
}
HeaderEnd();
$sth->finish;

#---------------------------------------------------------------------
$dbh->disconnect;
#======================================================================
sub OpenFile{
    my $arg =  shift;
    $arg=~s(^.*_)(); # rm Calibrations_ from "Calibrations_tpc"    if($arg=~m/\_/){
    $filename = "$arg" . 'Nodes.xml'; 
    $xml_fh = new FileHandle; 
    open $xml_fh, ">$filename";
}
sub NextLine{
    print $xml_fh "\n";
}
sub Tab{
    print $xml_fh "\t";
}
sub HeaderStart{
    MarkerStart($dbName,"StarDataBase"), NextLine();
}
sub HeaderEnd{
    MarkerEnd("StarDataBase");
}
sub MarkerStart{
    my ($value, $marker) = @_;
    print $xml_fh "<$marker> $value ";
}
sub MarkerEnd{
    my $marker = shift;
    print $xml_fh "</$marker>"; NextLine();    
}
sub MarkerBoth{
    my ($value, $marker) = @_;
    print $xml_fh "<$marker> $value </$marker> ";
}
sub Usage{
    print "\nUsage  : dbGetNode.pl -d database -s server\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
    print "                 -d database name\n";
    print "                 -h for this help\n\n";
    exit;
}


