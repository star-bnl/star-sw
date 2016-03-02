#!/usr/bin/env perl

#
# This is a command line utility to do a maintenance and cleanup
# of te FileCatalog database
#
# Initially written by Adam Kisiel, Warsaw University of Technology 
# in 2002 as part of a service task under J. Lauret.
#
# Maintained and modified by J.Lauret, BNL 2002-2013
#
# ---------------------------------------------------------------
# Warning note
# - This file was auto-generated as fC_cleanup.pl from a template
#   named fC_cleanup.new.pl
# - Do not edit directly but use the template as its content may
#   depend on other external includes and conditional formattings.
# ---------------------------------------------------------------
#
#

use Env qw(STAR_SCRIPTS);           
use lib "$STAR_SCRIPTS";            
#use lib "/star/u/jeromel/work/ddb"; # 
use strict;
use FileCatalog;
use Digest::MD5;

my @output;
my $i;
my $count;
my $debug;
my $delay;

# Control variables
# The size of a batch to process at once.
my $BATCHSIZE =  1000;
my $MARKSIZE  =  5000;
my $mode;
my $cond_list;
my $state="";
my $kwrd="";
my $newval=undef;
my $keyw;
my $n;
my $confirm;
my $allst;
my $asite;
my $class;
my $instance="";
my $limit;
my $sctx=0;
my $docache=0;

# Connection parameters
my $user=undef;
my $passwd=undef;
my $host=undef;
my $port=undef;
my $db=undef;

# global array
my @MARK;
my $ligne;

# Load the modules
my $fileC = FileCatalog->new();

# other vars
my $STARTT=undef;
my $TIMEOUT=undef;
my $OLDO;
my $FLNM=undef;
my $TMPF;


# Turn off module debugging and script debugging
$fileC->debug_off();
$debug    = 1;
$delay    = 0;
$mode     = 1;  # check is the default
$confirm  = 0;
$allst    = 0;
$asite    = 0;
$limit    = 1;
$docache  = 0;

# start timer
&CheckTime(0);

# Parse the command line arguments.
$count = 0;
while (defined $ARGV[$count]){
    if ($ARGV[$count] eq "-status"){
	$mode = 0;

    } elsif ($ARGV[$count] eq "-debug"){
	$fileC->debug_on();
    } elsif ($ARGV[$count] eq "-class"){
	# class of debugging
	$fileC->message_class($ARGV[++$count]);

    } elsif ($ARGV[$count] eq "-ucheck"){
	$mode = -1;
    } elsif ($ARGV[$count] eq "-check"){
	$mode = 1;

    } elsif ($ARGV[$count] eq "-nodebug"){
	$debug = 0;

    } elsif ($ARGV[$count] eq "-cache"){
	# unlikely we would use this options apart from testing
	# OR if we are standalone on one node
	$docache = 1;
    } elsif ($ARGV[$count] eq "-nocache"){
	$docache = 0;



    } elsif ($ARGV[$count] eq "-nl"){
	$limit = 0;

    } elsif ($ARGV[$count] eq "-i"){
	$instance  = $ARGV[++$count];
    } elsif ($ARGV[$count] eq "-t"){
	&CheckTime($ARGV[++$count]);
    } elsif ($ARGV[$count] eq "-u"){
	$user      = $ARGV[++$count];
    } elsif ($ARGV[$count] eq "-p"){
	$passwd    = $ARGV[++$count];
    } elsif ($ARGV[$count] eq "-h"){
	$host      = $ARGV[++$count];
    } elsif ($ARGV[$count] eq "-P"){
	$port      = $ARGV[++$count];
    } elsif ($ARGV[$count] eq "-db"){
	$port      = $ARGV[++$count];

    } elsif ($ARGV[$count] eq "-o"){
	# redirect ooutput to a file
	$TMPF = $FLNM = $ARGV[++$count];
	$TMPF =~ s/.*\///; $TMPF = "/tmp/$TMPF$$.tmp";

	open($OLDO, ">&STDOUT");
	open(STDOUT,">$TMPF"); 
	select(STDOUT); $| = 1;

	
    } elsif ($ARGV[$count] eq "-delete"){
	$mode      = 2;
	$BATCHSIZE = 250;
    } elsif ($ARGV[$count] eq "-cdelete"){
	print "-cdelete currently has the same meaning than -delete\n";
	$mode      = 2;
	$BATCHSIZE = 250;
    } elsif ($ARGV[$count] eq "-ddelete"){
	$mode      = 2;
	$delay     = 1;
	$BATCHSIZE = 250;

    } elsif ($ARGV[$count] eq "-alla"){
	if ($mode != 2){
	    print "-alla works only with deletion keywords\n";
	} else {
	    $mode = -$mode;
	}
    } elsif ($ARGV[$count] eq "-allst"){
	$allst = 1;
    } elsif ($ARGV[$count] eq "-asite"){
	$asite = 1;

    } elsif ($ARGV[$count] eq "-doit"){
	$confirm = 1;

    } elsif ($ARGV[$count] eq "-mark"){
	$mode  = 3;
	$state = $ARGV[++$count];


    } elsif ($ARGV[$count] eq "-alter" || $ARGV[$count] eq "-modif"){
	$mode = ($ARGV[$count] eq "-alter")?4:-4;
	($kwrd,$newval) = split("=",$ARGV[++$count]);

    } elsif ($ARGV[$count] eq "-recheck"){
	$mode = 5;
    } elsif ($ARGV[$count] eq "-fdata"){
	$mode = 6;
    } elsif ($ARGV[$count] eq "-floc"){
	$mode = 7;
    } elsif ($ARGV[$count] eq "-trgc"){
	$mode = 8;
    } elsif ($ARGV[$count] eq "-boots"){
	$mode = 9;
	$keyw = $ARGV[++$count];


    } elsif ($ARGV[$count] eq "-cond"){
	$cond_list = $ARGV[++$count];
	if ($debug > 0) { 
	    &Print("The conditions list is $cond_list"); 
	}

    } else {

	if ( $ARGV[$count] ne "-help" &&
	     $ARGV[$count] ne "--help"    ){
	    &Print("Wrong keyword used: ".$ARGV[$count]);
	}
	&Usage(1);
    }
    $count++;
}

if ($count == 0){  &Usage(1);}


#
# Handle instance for multi-process cohesion
#
if ( $instance ne ""){
    $instance = "/tmp/.fC_cleanup_$instance";
    if ( -e $instance ){
	my(@items)=stat($instance);
	if ( (time() - $items[9]) > 3600 ){
	    # No activity on lock file for an hour?
	    &Print("No activity detected on $instance, deleting");
	    unlink($instance);
	} else {
	    &Print("$instance exists, $$ exiting");
	    &Exit();
	}
    } 
}




my $morerecords = 1;
my $start = $count = 0;
while ($morerecords)
{
    #
    # Allow for one shot disable
    #
    my($test) = $ENV{HOME}."/fC_cleanup.quit";
    if ( -e $test ){
	# &Print("Found $test - quitting");
	&Die("Found $test - quitting");
    }
    if ( &CheckTime(-1) ){
	&Die("Timer reached @ $TIMEOUT seconds");
    }
    
    &Print("--- ($$)") if ($start == 0 && abs($mode) < 5);
    $morerecords = 0;

    if ($mode == 0){ 
	# -status
	# First mode of operation - just get the file list and their availability
	&MyConnect($fileC,"User")  if ($start == 0);
	&ResetContext($fileC);
	
	&Print("Checking mode=0 treated=$start (getting +$BATCHSIZE records) ".localtime());
	$fileC->set_context("limit=$BATCHSIZE");
	$fileC->set_context("startrecord=$start");
	$fileC->set_delimeter("::");

	# Getting the data
	@output = $fileC->run_query("path","filename","available");

	# Check if there are any records left
	if (($#output + 1) == $BATCHSIZE)
	  { $morerecords = 1; }

	# Printing the output
	foreach (@output)
	  {
	    my ($path, $fname, $av) = split ("::");
	    &Print(join("/",($path, $fname))." $av");
	}
	$start += $BATCHSIZE;



    } elsif ($mode*$mode == 1){  
        # -check/-ucheck
	# Second mode of operation - get the file list,
	# select the available ones and check if they
	# really exist - if not, mark them as unavailable later
	$SIG{INT} = 'IHandler';

	$n = 0;
	if ($start == 0){
	    &MyConnect($fileC,"User");
	} else {
	    if ($#MARK > $MARKSIZE ){
		$n += &DoMark($fileC,"User");
	    }
	}
	&ResetContext($fileC);
	&SiteContext($mode,$fileC);
	
	
	&Print("Checking mode=$mode checked=$start (getting +$BATCHSIZE records, cached=".($#MARK+1).") ".localtime());
	
	$fileC->set_context("limit=$BATCHSIZE");
	$fileC->set_context("startrecord=$start");
	if ($mode == 1){
	    $fileC->set_context("available>0");
	} else {
	    # ONLY = 0
	    $fileC->set_context("available=0");
	}
	$fileC->set_delimeter("::");

	my $store=$fileC->get_context("storage");
	if( ! defined($store) ){
	    # Impose NFS to minimize errors
	    $store = "NFS";
	    $fileC->set_context("storage=$store");
	} else {
	    if( $store eq "HPSS"){
		&Die("HPSS checking not immplemented yet");
	    }
	}


	# Getting the data
	@output = $fileC->run_query("path","filename","available","node","site");
	# @output = "/star/data34/reco/cuProductionMinBias/ReversedFullField/P07ib/2005/030::st_physics_adc_6030096_raw_2080008.tags.root::1::localhost::BNL";

	# Check if there are any records left
	# print "OUTPUT: $#output BATCHSIZE $BATCHSIZE\n";
	if (($#output +1) == $BATCHSIZE)
	  { $morerecords = 1; }

	# checking the availability
	# $n = 0;
	foreach (@output){
	    my ($path, $fname,$available,$node,$site) = split ("::");

	    # $fileC->clear_context();
	    # $fileC->set_context("filename=$fname");
	    # $fileC->set_context("path=$path");
	    # $fileC->set_context("storage=$store");
	    # $fileC->set_context("node=$node") if ($node ne "");
	    # $fileC->set_context("site=$site") if ($site ne "");

	    if ( ($mode == 1  && $available <= 0) ||
		 ($mode == -1 && $available != 0)){
		&Print("BOGUS logic or corrupt records !!");
		next;
	    }

	    # print "$path/$fname\n";
	    if ( &Exist("$path/$fname") ){
		if ($mode == -1){
		    # remark available
		    &Print("File  $site.$store://$node$path/$fname exists $available");
		    # $fileC->update_location("available",1,$confirm);
		    push(@MARK,"1:$confirm:$fname:$path:$available:$store:$node:$site");
		    # $n++;
		} elsif ($debug > 1){
		    &Print("Found  $site.$store://$node$path/$fname and avail=$available");
		}
	    } else {
		if ($mode == 1){
		    # and ! Exist($path/$file) that is ...
		    &Print("File  $site.$store://$node$path/$fname DOES NOT exist and available=$available");
                    # mark it un-available
		    # $fileC->update_location("available",0,$confirm);
		    push(@MARK,"0:$confirm:$fname:$path:$available:$store:$node:$site");
		    # print "Pushing 0:$confirm:$fname:$path:".":$store:$node:$site $#MARK\n";
		    # print $MARK[0]."\n";
		    # $n++;
		}
	    }

	}
	# &DoMark($fileC,"User");	
	
	# We have modified records so the next ROW number
	# need to be offset by how many we just changed
	$start += ($BATCHSIZE - $n) if ($n <= $BATCHSIZE);

    } elsif ($mode == 2 || $mode*$mode == 4){
	# -delete/-ddelete/-cdelete
	# Delete records. Note that this function is EXTREMELY
	# dangerous now since any record can be deleted based
	# on context.
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);
	&SiteContext($mode,$fileC);
	
	my($rec,@items);
	&Print("Checking mode=$mode treated=$count (getting +$BATCHSIZE records) ".localtime());
	$fileC->set_context("limit=$BATCHSIZE");
	$fileC->set_context("startrecord=$start");
	if ($mode == -2){
	    $fileC->set_context("available<0");
	} else {
	    $fileC->set_context("available=0");
	}
	$fileC->set_delimeter("::");
	$fileC->set_delayed() if ($delay);

	my $store=$fileC->get_context("storage");
	if( ! defined($store) && ! $allst){
	    # Impose NFS to minimize errors
	    $store = "NFS";
	    $fileC->set_context("storage=NFS");
	    &Print("NOTE :: storage=NFS is set by default. Use storage keyword to over-write");
	}


	if( $confirm ){
	    @items = $fileC->delete_records($confirm);
	    if( $debug ){
		foreach (@items){
		    &Print("Deleted $_");
		}
	    }

	    if ( $delay){
		# $fileC->print_delayed();
		$fileC->flush_delayed();
	    }
	} else {
	    @items = $fileC->run_query("site","node","storage","path","filename","available");
	    foreach (@items){
		&Print("$_");
	    }
	}

	if (($#items +1) == $BATCHSIZE){
	    $count += ($#items+1);
	    $morerecords = 1;
	} else {
	    &Print("Running post-deletion bootstrap");
	    &FullBootstrap($confirm);
	}
	if ($confirm){
	    # we have shifted records
	    $start += ($BATCHSIZE - $#items -1) if ($#items < $BATCHSIZE);
	} else {
	    $start += $BATCHSIZE;
	    &Print("Use -doit to confirm deletion");
	}



    } elsif ( $mode == 3 ){
	# -mark
	# Fourth mode of operation - mark selected files as available/unavailable
	# without checking if they exist or not.
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);
	&SiteContext($mode,$fileC);

	if ($debug>0){
	    if ($state eq "on"){
		&Print("Marking files as available");
	    } elsif ($state eq "off") {
		&Print("Marking files as unavailable");
	    } elsif ($state eq "delf") {
		&Print("Marking files as deletable (any dameon may pic those and delete)");
	    } else {
		&Print("Marking files with available = $state");
	    }
	}

	# Marking the file as unavailable
	$fileC->set_context("limit=0");

	if ($state eq "on"){
	    $fileC->set_context("available=0");
	    $fileC->update_location("available",1,$confirm);
	} elsif ( $state eq "off") {
	    $fileC->set_context("available=1");
	    $fileC->update_location("available",0,$confirm);
	} elsif ( $state eq "delf") {
	    $fileC->set_context("available=1");
	    $fileC->update_location("available",-1,$confirm);
	} else {
	    $fileC->set_context("available=1");
	    $fileC->update_location("available",$state,$confirm);
	}


    } elsif ( $mode*$mode == 16){
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);
	
	$fileC->set_context("limit=$BATCHSIZE");
	if ($kwrd ne ""){
	    if( ! defined($newval) ){
		&Die("  You must specify a new value");
	    } else {
		&Print("Resetting keyword [$kwrd] to new value $newval $start ".localtime());
	    }
	} else {
	    &Die("Keyword is empty ");
	}

	$fileC->set_delayed() if (! $confirm);
	if ($mode == 4){
	    $morerecords = $fileC->update_record($kwrd,"$newval",$confirm);
	} else {
	    my $delete=1;
	    $morerecords = $fileC->update_location($kwrd,$newval,$confirm,$delete);
	}
	if (! $confirm ){
	    $morerecords = 0;
	    &Print("Confirmed = $confirm . Use -doit for really updating");
	    $fileC->print_delayed();
	}
	# only for stat
	$start += $morerecords;
	# print "$morerecords\n";

    } elsif ($mode == 5){
	# -recheck
	# Fifth mode of operation - get the file list,
	# and check if they really exist - if not, mark them as unavailable
	# if yes - remark them as available
	$SIG{INT} = 'IHandler';

	$n = 0;
	if ($start == 0){	
	    &MyConnect($fileC,"User");
	} else {
	    if ($#MARK > $MARKSIZE ){
		 $n += &DoMark($fileC,"User");
	    }
	}
	&ResetContext($fileC);
	&SiteContext($mode,$fileC);

	&Print("Checking mode=$mode checked=$start (getting +$BATCHSIZE records, cached=".($#MARK+1).") ".localtime());
	$fileC->set_context("limit=$BATCHSIZE");
	$fileC->set_context("startrecord=$start");
	$fileC->set_context("all=1");
	$fileC->set_delimeter("::");


	my $store=$fileC->get_context("storage");
	if( ! defined($store) ){
	    # Impose NFS to minimize errors
	    $store = "NFS";
	    $fileC->set_context("storage=NFS");
	} else {
	    if( $store eq "HPSS"){
		&Die("HPSS checking not implemented yet");
	    }
	}

	# Getting the data
	@output = $fileC->run_query("path","filename","available","node","site");

	# Check if there are any records left
	# print "OUTPUT: $#output BATCHSIZE $BATCHSIZE\n";
	if (($#output +1) == $BATCHSIZE)
	  { $morerecords = 1; }

	# checking the availability
	# $n = 0;
	foreach (@output){
	    my ($path, $fname, $av,$node, $site) = split ("::");
	    if ( &Exist("$path/$fname") ){
		if ($av == 0){
		    # if ($debug > 0)
		    # {
		    &Print("File $site.$store://$node$path/$fname exists");
		    # }
		    # Marking the file as available
		    # $fileC->clear_context();
		    # $fileC->set_context("filename=$fname");
		    # $fileC->set_context("path=$path");
		    # $fileC->set_context("available=0");
		    # $fileC->set_context("storage=$store");
		    # $fileC->set_context("node=$node") if ($node ne "");
		    # $fileC->set_context("site=$site") if ($site ne "");
		    # $n += $fileC->update_location("available",1);
		    push(@MARK,"1:".":$fname:$path:0:$store:$node:$site")		    
		}
	    } else {
		if ($av == 1){
		    # if ($debug>0)
		    # {
		    &Print("File $site.$store://$node$path/$fname DOES NOT exist and available=1");
		    # }
		    # Marking the file as unavailable
		    # $fileC->clear_context();
		    # $fileC->set_context("filename=$fname");
		    # $fileC->set_context("path=$path");
		    # $fileC->set_context("available=1");
		    # $n += $fileC->update_location("available",0);
		    # NOTE: No store before (???)
		    push(@MARK,"0:".":$fname:$path:1:$store:$node:$site")		    		    
		}
	    }
	}
	$start += ($BATCHSIZE - $n) if ($n <= $BATCHSIZE);


    } elsif ($mode == 6) {
	# -fdata
	# Bootstrap FileData for orphan records
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);

	my @rows;
	$fileC->set_context("limit=100000000");
	@rows = $fileC->bootstrap("filename",$confirm);
	&Print("Returned IDs $#rows: @rows");
	&Print("Use -doit to do a real cleaning") if (! $confirm);

    } elsif ($mode == 7){
	# -floc
	# Bootstrap FileLocations for orphan records
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);
	
	my @rows;
	$fileC->set_context("limit=100000000");
	@rows = $fileC->bootstrap("owner",$confirm);
	&Print("Returned IDs $#rows: @rows");
	&Print("Use -doit to do a real cleaning") if (! $confirm);

    } elsif ($mode == 8){
	# -trgc
	# Bootstrap TriggerCompositions table (may take a while)
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);

	my @rows;
	$fileC->set_context("limit=100000000");
	@rows = $fileC->bootstrap("tctwid",$confirm);
	&Print("Returned IDs $#rows: @rows");
	&Print("Use -doit to do a real cleaning") if (! $confirm);

    } elsif ($mode == 9){
	# -boots
	# Boostrap a few tables (all) or a specific keyword
	&MyConnect($fileC,"Admin")  if ($start == 0);
	&ResetContext($fileC);

	if ( $keyw eq "all"){
	    &FullBootstrap($confirm);
	} else {
	    my @rows;
	    $fileC->set_context("limit=100000000");
	    @rows = $fileC->bootstrap($keyw,$confirm);
	    &Print("Returned IDs $#rows: @rows");
	}
	&Print("Use -doit to do a real cleaning") if (! $confirm);
    }
}


# need to work on the last batch of files not yet treated
if ($#MARK != -1){
    &DoMark($fileC,"User");
}
$fileC->destroy();

&Exit();


#
# Signal handler routine for CTRL/C
# If records are cached for marking, mark them before Exit()
#
sub IHandler
{
    $SIG{INT} = sub { exit };
    print STDERR "IHandler - Caught CTRL/C [1 sec to CTRL/C again and abort clean-up]\n";
    sleep(1);
    if ($#MARK != -1){
	print STDERR "IHandler - Marking records\n";
	&DoMark($fileC,"User");
    } else {
	print STDERR "IHandler - Nothing to mark\n";
    }
    &Exit();
}


#
# This will destroy i.e. close the FileCatalog, re-open as admin
# and start a batch marking
#
sub DoMark
{
    my($fileC,$wasas)=@_;
    my($count)=0;
    my($ligne,$Tstart,$rate,$delta);
    
    if ($#MARK != -1){
	if ($wasas ne "Admin"){
	    # this means we need to close and re-open as admin
	    $fileC->destroy();
	    # $fileC->debug_on();
	    &MyConnect($fileC,"Admin");
	    &Die("NULL FC handler")      if (!$fileC);
	    &Print("Now marking a batch of ".($#MARK+1)." files (switching to Admin)");
	} else {
	    &Print("Now marking a batch of ".($#MARK+1)." files (was Admin, re-using db connection)");
	}
	# &ResetContext($fileC);
	
	$Tstart=time();
	# $fileC->set_delayed();
	$fileC->warn_if_duplicates(0); # we disable because of delays between User and Admin
	foreach $ligne (@MARK){
	    # print "Got [$ligne]\n";
	    my($avail,$confirm,$fname,$path,$av,$store,$node,$site) = split(":",$ligne);
	    # print "Retreiving $avail,$confirm,$fname,$path,$av,$store,$node,$site\n";
	    # die;
	    last if ( &CheckTime(-1) );
	    
	    # $fileC->debug_on();
	    $fileC->clear_context();
	    $fileC->set_context("filename=$fname");
	    $fileC->set_context("path=$path");
	    $fileC->set_context("available=$av")   if ($av   ne "");
	    $fileC->set_context("storage=$store");
	    $fileC->set_context("node=$node")      if ($node ne "");
	    $fileC->set_context("site=$site")      if ($site ne "");
	    if ($confirm eq ""){  $confirm = undef;}
	    if ( $fileC->update_location("available",$avail,$confirm) ){
		$count++;
		$delta= time()-$Tstart;
		if ($delta != 0){
		    $rate = sprintf("%.4f",60*$count/(time()-$Tstart));
		} else {
		    $rate = "unknown";
		}
		&Print("Marked $fname $path ($rate ops/mn)"); # $store $node $site
	    }
	}
	# &Print("Flushing $count delayed operations ".localtime());
	# $fileC->flush_delayed();
	# $fileC->unset_delayed();
	# $rate = sprintf("%.4f",60*$count/(time()-$Tstart));
	
	# now re-connect as a normal non-Admin
	if ($wasas ne "Admin"){
	    # re-connect as initial $wasas
	    $fileC->destroy();
	    &MyConnect($fileC,$wasas) 
	}
	$fileC->warn_if_duplicates(1);
	undef(@MARK);
    }
    return $count;
}


sub MyConnect
{
    my($fileC,$as)=@_;
    my($luser,$lpasswd,$lport,$lhost,$ldb);
    
    if ( ! defined($as) ){  $as = "User";}
    
    # Get connection fills the blanks while reading from XML
    # However, USER/PASSWORD presence are re-checked
    my ($USER,$PASSWD,$PORT,$HOST,$DB) = $fileC->get_connection($as);
    
    $luser   = defined($user)?$user:$USER;
    $lpasswd = defined($passwd)?$passwd:$PASSWD;
    $lport   = defined($port)?$port:$PORT;
    $lhost   = defined($host)?$host:$HOST;
    $ldb     = defined($db)?$db:$DB;

    if ( $luser eq "" ){
	# get it from command line
	print "Username for FileCatalog : ";
	chomp($luser = <STDIN>);
    }
    if ( $lpasswd eq "" ){
	# get it from command line
	print "Password for $user : ";
	chomp($lpasswd = <STDIN>);
    }

    # Load is managed globally - remove limit if -nl
    if ( !$limit && $as eq "Admin"){
	$fileC->set_thresholds(0,0,0);
    }
    

    #
    # Now connect
    #
    if ( ! $fileC->connect($luser,$lpasswd,$lport,$lhost,$ldb) ){
	&Print("Failed to connect to db");
	&Exit();
    }
}

sub ResetContext
{
    my($fileC)=@_;
    
    # Setting the context based on the swiches
    $fileC->clear_context();
    if (defined $cond_list){
	foreach (split(/,/,$cond_list)){
	    $fileC->set_context($_);
	}
    }
    $fileC->set_context("cache=0") if ( ! $docache);  # we disable cache by default

}

#
# Set site context based on ENV and options
#
sub SiteContext
{
    my($mode,$fileC)=@_;

    if (! $asite){
	my $site=$fileC->get_context("site");
	my $tmp;

	if ( ! defined($ENV{DOMAINNAME}) ){
	    # try defining it
	    if ( -x "/bin/domainname"){
		chomp($tmp = `/bin/domainname`);
		$ENV{DOMAINNAME} = $tmp;
	    } elsif ( -x "/bin/hostname"){
		chomp($tmp = `/bin/hostname`);
		$tmp =~ s/^[^\.]*\.//;
		$ENV{DOMAINNAME} = $tmp;
	    }
	}
	#print "DEBUG $ENV{DOMAINNAME} \n";

	if ( ! defined($site) ){
	    # try guessing
	    if ( $ENV{DOMAINNAME} =~ m/bnl.gov/ || $ENV{HOSTNAME} =~ m/bnl.gov/){
		$fileC->set_context("site=BNL");
		&Print("Restricting to site=BNL") if (! $sctx);
	    } elsif ( $ENV{DOMAINNAME} =~ m/nersc.gov/ || $ENV{HOSTNAME} =~ m/nersc.gov/){
		$fileC->set_context("site=LBL");
		&Print("Restricting to site=LBL") if (! $sctx);
	    } else {
		print qq~
 Site cannot be guessed, please specify the site context OR report to the  
 script developper for extension. Information you need to porvide includes
 the possible of the environment variable DOMAINNAME or some possible values
 for the environment variable HOSTNAME.
 ~;
		&Die();
	    }
	}
    } else {
	if ( $mode*$mode == 1 && ! $sctx){
	    &Print("WARNING check/ucheck with all site is dangerous - pausing 10 seconds, CRT/C to abort");
	    sleep(10);
	}
    }
    $sctx = 1;
}


sub Die
{
    my($mess)=@_;
    &Exit("$mess",1);
}
sub Exit
{
    my($val,$mode)=@_;

    if ( !defined($mode)) { $mode = 0;}
    if ( !defined($val))  { $val  = ($mode==0?0:"killed");} 
    
    $fileC->destroy() if ( $fileC);
    unlink($instance) if ( $instance ne "");
    
    if ($TMPF ne ""){
	close(STDOUT);
	open(STDOUT, ">&$OLDO");

	# rename will not work as across FS
	# print "/bin/cp -fp $TMPF $FLNM\n";
	system("/bin/cp -fp $TMPF $FLNM");

    }
    if ($mode == 1){
	die "$val\n";
    } else {
	exit $val;
    }
}


sub Print
{
    my(@msg)=@_;
    my($mess);
    foreach $mess (@msg){
	chomp($mess);
	print "$mess\n";
	if ( $instance ne ""){
	    open(OUT,">>$instance");
	    print OUT localtime()." $$ $mess\n";
	    close(OUT);
	}
    }
}



sub Exist
{
    my($file)=@_;
    my($realf);

    # DO support soft-links
    if ( -l $file ){
	# only files in a path containing starreco/reco will 
	# apply otherwise, we will ignore. 
	# Path like /home/starlib/home/starreco should be valid 
	# and /home/starlib/reco not ... This is a Xrootd 
	# transition hack.
	if ( $file =~ m/starlib\/reco/){ return 0;}

	# check if target exists
	$realf = readlink($file);
	if ( -e $realf ){
	    return 1;
	} else {
	    return 0;
	}
    } elsif ( -e $file ){
	return 1;
    } else {
        # and since I am paranoid
	if ( open(FT,$file) ){
	    close(FT);
	    return 1;
	} else {
	    return 0;
	}
    }
}


sub FullBootstrap
{
    my($confirm)=@_;
    my(@rows);

    $fileC->set_context("limit=100000000");
    $fileC->set_context("cache=0") if ( ! $docache);
    @rows = $fileC->bootstrap("runnumber",$confirm);
    @rows = $fileC->bootstrap("collision",$confirm);
    @rows = $fileC->bootstrap("generator",$confirm);
    @rows = $fileC->bootstrap("configuration",$confirm);
    @rows = $fileC->bootstrap("path",$confirm);
    @rows = $fileC->bootstrap("node",$confirm);
}


sub Usage
{
    my($sts)=@_;

  # Formatted by text2c V01-122 (Flex V2.5). Last update 31-Dec-2003 
  print "\n Usage: fC_cleanup [option] -cond field=value{,field=value}";
  print "]\n\n The condition list may be listed using the get_file_lis";
  print "t.pl script.\n\n where option is one of\n\n General modes of ";
  print "operation (status check)\n ----------------------------------";
  print "--------\n -status                 show availability status f";
  print "or each file\n\n -check                  check files with ava";
  print "ilability > 0, set to 0 if not\n                         foun";
  print "d\n -ucheck                 check files with availability=0, ";
  print "set to 1 if found\n -recheck                recheck all files";
  print " (any availability) and adjust as\n                         n";
  print "ecessary\n\n -mark {on|off|delf|i}   mark selected files avai";
  print "lability (1, 0 or the\n                         specified val";
  print "ue i)\n\n Caveats:\n * Unless specified, the context storage ";
  print "is set to NFS by default for check\n   operations. HPSS check";
  print "s are NOT implemented.\n * The default site will be guessed o";
  print "r the command will abort\n\n marking operations are not auto-";
  print "restricted to a storage context and you may\n want to specify";
  print " a condition.\n\n\n Potentially damaging mode of operation\n ";
  print "---------------------------------------\n -delete/-{c|d}delet";
  print "e    delete records with availability=0 (current context\n   ";
  print "                      applies). cdelete only checks it but do";
  print " not do\n                         anything. ddelete uses the ";
  print "delayed mechanism of the\n                         module\n\n\n";
  print " WARNING!!! The delete operation is irreversible ! Therefore,";
  print " we made it\n  particularly cumbersome to delete records to p";
  print "revent accidental delete.\n  DO NOT use the below keywords un";
  print "less you are sure of what will happen.\n\n  -doit      switch";
  print " MUST now be specified for ALL delete operations\n\n  -allst ";
  print "    Unless specified, the context storage is set to NFS by de";
  print "fault for\n             delete operations. -allst is a switch";
  print " you may want to use to take\n             into account all s";
  print "torage type in one shot. The default storage=NFS\n           ";
  print "  is made to prevent accidental deletion of persistent stored";
  print " files.\n             This should be used with caution.\n\n  ";
  print "-alla      is a switch you may use to affect availability < 0";
  print ". The default\n             is to only delete the 0 ones. Pra";
  print "ctically, -ucheck would check\n             for files previou";
  print "sly marked unavailable (on more check) and\n             -del";
  print "ete -alla would delete all records marked with any\n         ";
  print "    availability <= 0.\n\n Other options to use with extreme ";
  print "care\n\n -alter keyword=value    alter keyword value for entr";
  print "y(ies) ** DANGEROUS **\n                         This works o";
  print "n dictionaries or tables and may allows\n                    ";
  print "     for global updates.\n -modif keyword=value    alter File";
  print "Data/FileLocation association for entry(ies)\n               ";
  print "          This switch also modify non-dictionary tables but\n ";
  print "                        within careful checks (one by one).\n\n";
  print " Integrity check operations\n --------------------------\n Th";
  print "e -doit switch MUST also be specified for the below options\n ";
  print "-floc                   check the FileLocations for orphan re";
  print "cords (no \n                         associated Data)\n -fdat";
  print "a                  check the FileData for orphan records (no ";
  print "associated\n                         Locations)\n -trgc      ";
  print "             check the TriggerCompositions table\n -boots {X|";
  print "all}          bootstrap keyword X, using \"all\" will do a se";
  print "quence of\n                         table cleaning (but not f";
  print "ilename or flid)\n\n Authentication options\n ---------------";
  print "-------\n -u user                 use 'user' db access privs\n";
  print " -p passwd               use 'password' for db access\n -h ho";
  print "st                 use 'host' for db access\n -P port        ";
  print "         use 'port' for db access\n -db db                  u";
  print "se database 'db' for db access\n\n Other options\n ----------";
  print "---\n -debug            turn database module debugging inform";
  print "ation ON  default=OFF)\n -nodebug          turn this script d";
  print "ebugging information OFF (default=ON)\n -help --help      pri";
  print "nt this help\n\n -o file           Redirect all standard outp";
  print "ut to 'file'\n\n -doit             switch MUST be specified t";
  print "o really perform the operation.\n                   Without i";
  print "t, the API will only display the operation it\n              ";
  print "     intends to perform (i.e. debug mode). It is wise to star";
  print "t\n                   in debug mode.\n\n -cache            Us";
  print "e disk cache for querries (default is to disable it)\n -nocac";
  print "he          Do not use disk caching (default)\n\n -nl        ";
  print "       Bypass the load balancer - do not use this option in c";
  print "ron\n -t time           Terminate gracefully after approximat";
  print "ely 'time' has elapsed\n -class XXX        Set debugging clas";
  print "s\n\n";

    if( defined($sts) ){ &Exit();}

}

# return true false if we have timed-out
# Call with  0 to initialize the timer
# Call with a value > 0 to initialize timeout time
# Call with -1 to check timer
sub CheckTime
{
    my($w)=@_;

    if ($w == 0){
        # Init timer
        $STARTT = time();
    } elsif ($w > 0){
        # set timeout
        $TIMEOUT = $w;
    } else { # < 0
        return 0 if ( ! defined($TIMEOUT) || ! defined($STARTT) );
        return (time()-$STARTT) > 0.9*$TIMEOUT; # 90% to be sure
    }
}

