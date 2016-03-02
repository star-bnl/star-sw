#!/usr/bin/perl 
#
#
# $Id: dbGetSchema.pl,v 1.3 2007/03/28 04:18:25 deph Exp $
#
# Author: Bum Choi & R. Jeff Porter
#
#***************************************************************************
#
# Description: parses a table from mysql into an xml format
#
#****************************************************************************
# 
# $Log: dbGetSchema.pl,v $
# Revision 1.3  2007/03/28 04:18:25  deph
# Added quotes to identifier schema
#
# Revision 1.2  2003/01/09 20:30:27  porter
# upgrade of db table structure scripts
#
# Revision 1.1  2000/04/28 14:08:04  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################
#!/usr/bin/perl 
#
# parses c structs from mysql into an xml format
# 
#===================================================================
# package StructToXml; (maybe later)
#===================================================================
use DBI;
#use strict;
use Getopt::Std;
use FileHandle;
#===================================================================
# -d database name -n structName -i schemaId -h help 
getopts('hd:n:s:i:');
my $dbName     = $opt_d; 
my $structName = $opt_n; 
my $schemaID   = $opt_i; 
my $serverHost = $opt_s;

($opt_h or not defined $dbName or not defined $serverHost) and Usage();
#-------------------------------------------------------------------
my ($dbh, $sth1, $sth2);  
my ($name, $type, $length, $comment, $structID, $lastID);
my (@row, $query, @checkarray);
my $xml_fh;  # handle used in all the print statements
#-------------------------------------------------------------------
$dbh = DBI->connect("DBI:mysql:$dbName:$serverHost",		
       {RaiseError => 1, AutoCommit => 0})
  or die "couldnt connect to $dbName: $dbh->errstr\n";
#------------------------------------------------------------------- 
if (defined $structName){
    $query = qq{select name, comment, id, lastschemaid } .
	     qq{from structure where name='$structName'};
} else {
    $query = qq{select name, comment, id, lastschemaid from structure}; 
}
$sth1=$dbh->prepare($query);   
$sth1->execute;    
$sth2=$dbh->prepare(qq{select name, type, length, comment from `schema` }. 
		    qq{where structID=? and schemaID=?});
#--------------------------------------------------------------------
while(($structName, $comment, $structID, $lastID) = $sth1->fetchrow_array){
    if (defined $schemaID) {
	$sth2->execute($structID, $schemaID);
	@checkarray = CheckSchema($structID, $schemaID);
	defined @checkarray 
	    or die "No $structName with schema $schemaID";
    } else {$sth2->execute($structID, $lastID);}

    OpenFile($structName);
    HeaderStart(); 
    MarkerStart($structName,"StDbTable");
    MarkerBoth($comment,"comment") if $comment =~ /\w+/;
    NextLine();
    PrintData(@row) while (@row = $sth2->fetchrow_array);
    MarkerEnd("StDbTable");
    HeaderEnd();						  
}
$sth2->finish;
$sth1->finish;		   
#---------------------------------------------------------------------
$dbh->disconnect;
#======================================================================
sub OpenFile{
    my $arg =  shift;
    $filename = "$arg" . '.xml'; 
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
sub MarkerTypeStart{
    my ($value, $marker) = @_;
    MarkerStart($value, ModifyMarker($marker));
}
sub MarkerTypeEnd{
    my $marker = shift;
    MarkerEnd(ModifyMarker($marker));
}
sub ModifyMarker{
    my $arg = shift;
    $arg =~ s/(^\w)/\U$1/;
    return "db" . $arg;
}
sub PrintData{
    my ($name, $type, $length, $comment) = @_;
    Tab(); MarkerTypeStart($name, $type);
    MarkerBoth($length,"length") if $length > 1;
    MarkerBoth($comment,"comment") if $comment =~ /\w+/;
    MarkerTypeEnd($type);
}
sub CheckSchema{
    my ($structid, $schemaid) = @_;
    my @checkarray = 
	    $dbh->selectrow_array(qq{select structID from `schema` }.
				  qq{where structID=$structid and }.
				  qq{schemaID=$schemaid});
    return @checkarray;
}
sub Usage{
    print "\n";
    print "****  Usage   :: dbGetSchema.pl -d database -s server [-n structName -i schemaID]\n";
    print "****  Purpose :: writes xml file(s) version of c-struct schema from database \n";
    print "****  Defaults:: all structures, last schemaID, file='structName.xml'\n\n";
    print "                 -d database for requests\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                   'hostname' if portnumber=3306\n";
    print "                 -h for this help\n\n";
    exit;
}





