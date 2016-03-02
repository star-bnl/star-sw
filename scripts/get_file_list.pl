#!/opt/star/bin/perl
##!/usr/bin/env perl

#
# This is the command line utility, which allows access to the data in the
# FileCatalog database.
#
# Initially written by Adam Kisiel, Warsaw University of Technology
# in 2002 as part of a service task under J. Lauret.
#
# Written by J.Lauret 2002-2014
#
# Uncodumented paramaters
#   -debug      : maintainance option
#   -coffee     : maintainance option
#
# ---------------------------------------------------------------
# Warning note
# - This file was auto-generated as get_file_list.pl from a template
#   named get_file_list.new.pl
# - Do not edit directly but use the template as its content may
#   depend on other external includes and conditional formattings.
# ---------------------------------------------------------------
#
#
#use Env (OPTSTAR);
#use lib "$OPTSTAR/lib";
use lib "/afs/rhic.bnl.gov/star/packages/.DEV2/scripts";
use strict;
use FileCatalog;

my @output;
my $i;
my $count;
my ($debug,$sqlcaching,$docache );

# The state variables
my ($all, $alls, $unique, $field_list, $class);
my ($cond_list, $start, $limit, $rlimit, $delim, $onefile, $outfilename);
my ($intent)="User";

my $SELF = $0;

$SELF =~ s/.*\///;
$SELF =~ s/\..*//;

# Load the modules to store the data into a new database
# Simple way to connect (possibly using XML or default values)
my $fileC = FileCatalog->new();


# Set the defaults for he state values
$all         = 0;
$alls        = 0;
$unique      = 0;
$field_list  = "";
$cond_list   = "";
$fileC->debug_off();
$debug       = 0;
$onefile     = 0;
$outfilename = "";
$class       = "";
$sqlcaching  = 0;
$docache     = 1;

# Parse the cvommand line arguments.
$count = 0;

while (defined $ARGV[$count]){
    if ($ARGV[$count] eq "-all"){
	$all = 1;
    } elsif ($ARGV[$count] eq "-alls") {
	$alls = 1;
    } elsif ($ARGV[$count] eq "-onefile"){
	$onefile = 1;
    } elsif ($ARGV[$count] eq "-distinct"){
	$unique = 1;
	&PDebug("-distinct is obsolete");
    } elsif ($ARGV[$count] eq "-sqlcache"){
	$sqlcaching= 1;
    } elsif ($ARGV[$count] eq "-cache"){
	$docache= 1;
    } elsif ($ARGV[$count] eq "-nocache"){
	$docache= 0;

    } elsif ($ARGV[$count] eq "-V"){
	print "This is Version ".$fileC->Version()."\n";
	exit;
    } elsif ($ARGV[$count] eq "-debug" ||
	     $ARGV[$count] eq "-coffee"){
	$debug++;
	if ( $ARGV[$count] eq "-coffee"){
	    # full debugging not only this script
	    $fileC->debug_on();
	}

    } elsif ($ARGV[$count] eq "-class"){
	# class of debugging
	$fileC->message_class($ARGV[++$count]);

    }elsif ($ARGV[$count] eq "-as")
      { $intent = $ARGV[++$count]; }


    elsif ($ARGV[$count] eq "-start")
      { $start = $ARGV[++$count]; }
    elsif ($ARGV[$count] eq "-limit")
      { $limit = $ARGV[++$count]; }
    elsif ($ARGV[$count] eq "-rlimit")
      { $rlimit = $ARGV[++$count]; }
    elsif ($ARGV[$count] eq "-delim")
      {	$delim = $ARGV[++$count]; }
    elsif ($ARGV[$count] eq "-keys")
      {
	$field_list = $ARGV[++$count];
	&PDebug("The field list is $field_list");
      }
    elsif ($ARGV[$count] eq "-cond")
      {
	$cond_list = $ARGV[++$count];
	&PDebug("The conditions list is $cond_list");
      }
    elsif ($ARGV[$count] eq "-o")
    {
	$outfilename = $ARGV[++$count];
    }
    else
    {
	if ( #$ARGV[$count] ne "-h"    && -h was used for host selection before
	     $ARGV[$count] ne "-help" &&
	     $ARGV[$count] ne "--help"    ){
	    print "Unknown switch used: ".$ARGV[$count]."\n" ;
	}
	&Usage();
	exit;
    }
    $count++;
}

if ($count == 0){
    &Usage();
} else {
    # connect_as() in this case mandate that the rest of the
    # the information is defined via connection schema.
    $fileC->connect_as($intent);

    if ($outfilename ne ""){
	open (STDOUT, ">$outfilename") || die "Cannot redirect output to file $outfilename";
    }

    # Setting the context based on the switches
    foreach (split(/,/,$cond_list)){
	$fileC->set_context($_);
    }
    if ($all ==1){         $fileC->set_context("all=1");   }
    if ($alls==1){
        # do nothing
    } else {
	# do something only if sanity was not used
	# in the condition
	if ( ! defined($fileC->get_context("sanity")) ){
	    $fileC->set_optional_context("sanity=1");
	}
    }
    if (defined $limit){   $fileC->set_context("limit=$limit"); }
    if (defined $rlimit){  $fileC->set_context("rlimit=$rlimit"); }
    if (defined $start){   $fileC->set_context("startrecord=$start"); }
    if (defined $delim){   $fileC->set_delimeter($delim); }
    if ($unique==0){       $fileC->set_context("nounique=1");}
    if (! $docache){       $fileC->set_context("cache=0") ;}    # cache is enabled by default
                                                                # we disable cache if asked

    if ($onefile > 0){
        # ,orda(persistent)"; <-- great idea but returns persistent
	$field_list .= ",grp(filename),orda(persistent)";
    }


    &PDebug("Full context is ".$fileC->get_context());

    # Getting the data - DO NOT use query_cache() for all querries
    if ($sqlcaching){
	@output = $fileC->run_query_cache(split(/,/,$field_list));
    } else {
	@output = $fileC->run_query(split(/,/,$field_list));
    }

    # This requires version V01.395 or above
    # Note that file based query caching is enabled by the keyword cache=1
    if ($fileC->was_file_cache_used() ){
	&PDebug("File based query cache was used");
    }

    # Printing the output
    if ($onefile == 0) {
	foreach (@output){ print "$_\n"; }

    } else {
	my (@fields);
	my ($delimeter,$line,$cline);
	my ($lastfname) = "";

	if (defined $delim){
	    $delimeter = $delim;
	} else {
	    $delimeter = "::";
	}
	# not secure to do this with split() as delim can be
	# changed.
	foreach $line (@output){
	    for ( $i= length($line) ; $i > 0 ; $i--){
		if ( substr($line,$i,length($delimeter)) eq $delimeter){ last;}
	    }
	    $cline = substr($line,0,$i);

	    if ($cline ne $lastfname){
		print "$cline\n";
	    }
	    $lastfname = $cline;
	}
    }
}

sub PDebug
{
    my(@msg)=@_;
    my($m);

    if ( $debug > 0){
	foreach $m (@msg){
	    chomp($m);
	    printf("%20.20s :: %s\n","$SELF($$)",$m);
	}
    }
}



sub Usage
{
  # Formatted by text2c V01-122 (Flex V2.5). Last update 31-Dec-2003
  print "\nCommand usage:\n %% get_file_list.pl [qualifiers] -keys fie";
  print "ld{,field} [-cond field=value{,field=value}]\n\n where the qu";
  print "alifiers may be\n -all                  use  all entries rega";
  print "rdless of availability  flag \n                       default";
  print " is available=1\n -alls                 use all entries regar";
  print "dless of sanity flag \n                       default is sani";
  print "ty=1 unless the  sanity key was used \n                      ";
  print " as a condition\n\n -onefile              returns only one lo";
  print "cation (slow)\n\n -delim <string>       sets the default deli";
  print "meter in between keys\n\n -limit <Num>          limits  the n";
  print "umber of returned records to Num \n                       (us";
  print "e 0 for all records, 100 is the default value)\n -rlimit <Num";
  print ">         limits the number of unique LFN to Num (number of \n";
  print "                       returned lines will be more). rlimit w";
  print "ill switch \n                       limit logic off.\n -start";
  print " <Num>          start at the n-th record of the sample for ei";
  print "ther \n                       limit or rlimit mode\n\n -cache";
  print "                Use disk cache for querries (default)\n -noca";
  print "che              Do not use disk caching \n -sqlcache        ";
  print "     attempt to use server side cache (expert mode, the \n   ";
  print "                    module should deternine if it can automat";
  print "ically)\n\n -o <output filename>  redirects results to an oup";
  print "ut file (default uses \n                       STDOUT)\n\n -a";
  print "s <scope>           connects as specified, scopes={Admin|User";
  print "}\n -as <site>::<scope>   connects to site as specified\n\n -";
  print "V                    print version and exits\n -help --help  ";
  print "        print this help\n\n Fields appearing in -keys and/or ";
  print "-cond may be amongst the following\n\n";
  print join(" ",$fileC->get_keyword_list())."\n\n";

}

