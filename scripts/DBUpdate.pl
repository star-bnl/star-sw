#!/opt/star/bin/perl -w

use Env qw(STAR_SCRIPTS);
use lib "$STAR_SCRIPTS/";
# use lib "/star/u/jeromel/work/STAR/scripts";
use SpiderUtil;
use FileCatalog;
use Date::Manip;

if ($#ARGV == -1){

    print qq~

 Syntax is
  % DBUpdate.pl [options] BasePath [fileExtension] [RelPathOverwrite] [Substitute]
     [user] [password]

 Options are
   -o outputFile      redirect output to file outputFile
   -of markerFile      on finding, create markerFile delete otherwise if exists

   -k subpath         strip subpath before comparing to HPSS, then clone
                      using full path
   -z                 use any filetype (not the default)
   -l                 consider soft-links in path
   -t ts              finish after approximately ts seconds

   -nocache           do not use caching (default)
   -dcache            delete cache entirely
   -cache             use caching which will allow to process the difference 
                      only saving querry cycles
   -coff Offsset      add the value Offset to the cache frequency


 Purpose
   This script scans a disk given as the first argument
   checks all files and update the database with a file
   location new entry if it finds the same entry as
   storage = HPSS.

   It uses the clone_location() method to update the
   database with that entry and is a very good example
   of how to do this ...

   This is REALLY a spider ... It is used to post-scan
   disk and catch entries which may be missing.

   Only clones (if there is no similar entries in the db,
   it will not add it).


 Arguments are
   ARGV0   the base path of a disk to scan (default /star/data06)
   ARGV1   the filetype (default .MuDst.root ). If null, it will
           search for all files
   ARGV2   this scripts limits it to a sub-directory "reco" starting
           from ARGV0. Use this argument to overwrite.
   ARGV3   A base path substitution for find the entry in HPSS
           Default is /home/starreco .

   ARGV4   a user name (default FC_admin)
   ARGV5   a password (default will be to use the
           get_connection() method as a guess try)


 Examples
  % DBUpdate.pl /star/data27
  % DBUpdate.pl /star/data27 ""
  % DBUpdate.pl /star/data03 .daq daq /home/starsink/raw
  % DBUpdate.pl /home/starlib/home/starreco -k /home/starlib -l

~;
    exit;
}

# BEWARE :
#  (1) $SITE and $HPSSD are global variables
#  (2) There is an hidden logic based on $path !~ /\/star\/data/
#      to recognized if the storage is local or NFS.

$SPDR   = new SpiderUtil();

$SITE   = "BNL";
$SELF   = "DBUpdate";
$LOUT   = 0;
$FLNM   = "";
$MARKERF="";

# Those default should nt be changed here but via
# command line options
$SCAND  = "/star/data06";
$USER   = "";
$PASSWD = "";
$SUBPATH= "";
$SUB    = "reco";
$DOSL   = 0;
$DOCACHE= 0;

# var recovered from module
$CHKDIR = $SPDR->GetWDir();
$HPSSD  = $SPDR->GetHPSSDir();
$DEBUG  = defined($ENV{SPDR_DEBUG})?$ENV{SPDR_DEBUG}:0;

# Argument pick-up
$kk     = 0;
$FO     = STDERR;
$|      = 1;

# will hold time for this pass
$ext    = 0;


# start timer
$SPDR->SetTimeThreshold(0.75);
$SPDR->CheckTime(0);


for ($i=0 ; $i <= $#ARGV ; $i++){
    # Support "-XXX" options
    #print "Looking at [$ARGV[$i]]\n";
    if ($ARGV[$i] eq "-o"){
	$FLNM = $ARGV[$i+1];

	# Be sure we check on the tmp file and do 
	# not have process clashing.
	if ( -e "$FLNM.tmp"){
	    my(@items)=stat("$FLNM.tmp");
	    my($deltatime)= time() - $items[9];
	    if ( $deltatime < 900){
                # this file is too recent i.e. less than 10 mnts
                if ( ! open(FO,">>$FLNM") ){
		    die "Failed to open $FLNM in append mode\n";
		}
                print FO 
		    "$FLNM.tmp detected and more recent than expected. ".
		    "Process $$ exit.\n";
                close(FO);
                exit;
	    } else {
		unlink("$FLNM.tmp");
	    }
	}
	if ( open(FO,">$FLNM.tmp") ){
	    $FO = FO;
	} else {
	    die "Failed to open $FLNM.tmp\n";
	}
	# if we were called and there is a .kill file, remove
	$FLNM =~ m/(.*)(\..*)/; $tmp = $1; 
	if ( -e "$tmp.kill"){
	    print "Deleting $tmp.kill\n";
	    unlink("$tmp.kill");
	}

	$i++;

    } elsif ($ARGV[$i] eq "-of"){
	$MARKERF= $ARGV[++$i];

    } elsif ($ARGV[$i] eq "-nocache"){
	$DOCACHE= 0;
    } elsif ($ARGV[$i] eq "-cache"){
	$DOCACHE= 1;
    } elsif ($ARGV[$i] eq "-dcache"){	
	$DOCACHE= -1;
	
    } elsif ($ARGV[$i] eq "-k"){
	$SUBPATH = $ARGV[++$i];

    } elsif ($ARGV[$i] eq "-t"){
	$SPDR->CheckTime($ARGV[++$i]);

    } elsif ($ARGV[$i] eq "-l"){
	$DOSL    = 1;

    } elsif ($ARGV[$i] eq "-z"){
	$FTYPE   = " ";

    } elsif ($ARGV[$i] eq "-coff"){
	$CACHOFF= int($ARGV[++$i]);
	if ( $CACHOFF == 0){
	    # use the default algorithm, adding a modulo which is 5 times the base
	    # based on the node name
	    chomp($HOST = `/bin/hostname`);

	    # this will initialize the caching values
	    $SPDR->ToFromCache(-2);
	    
	    # use it as seed
	    if ( $HOST =~ m/(\d+)/ ){
		$CACHOFF = ($1 % ($SPDR->SetCacheLimit()*5));
	    } else {
		# revert to 0
		$CACHOFF = 0;
	    }
	    $SPDR->SetCacheOffset($CACHOFF);
	    print "$SELF :: Cache offset is $CACHOFF\n";
	}

    } else {
	# ... as well as previous syntax
	$kk++;
	$SCAND = $ARGV[$i] if ( $kk == 1);
	$FTYPE = $ARGV[$i] if ( $kk == 2);
	$SUB   = $ARGV[$i] if ( $kk == 3);
	$HPSSD = $ARGV[$i] if ( $kk == 4);
	$USER  = $ARGV[$i] if ( $kk == 5);
	$PASSWD= $ARGV[$i] if ( $kk == 6);
    }
}


# Get shorten string for path or base path for HPSS regexp
#@items  = split("/",$SCAND);
#$SCANDS = "/".$items[1]."/".$items[2];

# Set cache name
$SPDR->SetCacheName($SCAND);


#@ALL =( "$SCAND/$SUB/FPDXmas/FullField/P02ge/2002/013/st_physics_3013016_raw_0018.MuDst.root",
#	"$SCAND/$SUB/FPDXmas/FullField/P02ge/2002/013/st_physics_3013012_raw_0008.MuDst.root");

$failed = $unkn = $old = $new = 0;
$DOIT   = ($#ALL == -1);


if ( ! defined($FTYPE) ){  $FTYPE = ".MuDst.root";}
$FTYPE =~ s/^\s*(.*?)\s*$/$1/;   # trim leading/trailing


#print "Scanning $SCAND/$SUB\n";

$stimer = time();
if( $DOIT && -e "$SCAND/$SUB"){
    if ($FTYPE ne "" ){
	print "Searching for all files like '*$FTYPE' in $SCAND/$SUB  ...\n";
	if ( $DOSL){
	    @ALL   = `/usr/bin/find $SCAND/$SUB -type l -name '*$FTYPE'`;
	    print "Found ".($#ALL+1)." links to add (x2)\n";
	} else {
	    @ALL   = `/usr/bin/find $SCAND/$SUB -type f -name '*$FTYPE'`;
	    print "Found ".($#ALL+1)." files to add (x2)\n";
	}

    } else {
	print "Searching for all files in $SCAND/$SUB ...\n";
	if ($DOSL){
	    @ALL   = `/usr/bin/find $SCAND/$SUB -type l`;
	    print "Found ".($#ALL+1)." links to add (x2)\n";
	} else {
	    @ALL   = `/usr/bin/find $SCAND/$SUB -type f`;
	    print "Found ".($#ALL+1)." files to add (x2)\n";
	}
    }
}
$etimer = time()-$stimer;

# cache deletion
if ( $DOCACHE == -1){     $SPDR->ToFromCache(-1); }


    
# Eventually, if nothing is to be done, leave now
if ($#ALL == -1){ goto FINAL_EXIT;}


# Added algo to process by differences
if ( $DOCACHE ){   @ALL = $SPDR->ToFromCache(0,@ALL);}





$fC = FileCatalog->new();


# Get connection fills the blanks while reading from XML
# However, USER/PASSWORD presence are re-checked
#$fC->debug_on();
($USER,$PASSWD,$PORT,$HOST,$DB) = $fC->get_connection("Admin");
$port = $PORT if ( defined($PORT) );
$host = $HOST if ( defined($HOST) );
$db   = $DB   if ( defined($DB) );


if ( defined($USER) ){   $user = $USER;}
else {                   $user = "FC_admin";}

if ( defined($PASSWD) ){ $passwd = $PASSWD;}
else {                   print "Password for $user : ";
                         chomp($passwd = <STDIN>);}





#
# Now connect
#
if ( ! $fC->connect($user,$passwd,$port,$host,$db) ){
    &Stream("Error: Could not connect to $host $db using $user (passwd=OK)");
    goto FINAL_EXIT;
}

#$fC->debug_off();

$fC->set_silent(1);                  # Turn OFF messaging
$fC->Require("V01.307");             # pathcomment and nodecomment requires a minimal version

# Make a main context
# Temporary so we get it once only
chomp($NODE    = `/bin/hostname`);
&Stream("Info : We are on $NODE started on ".localtime());


undef(@TEMP);  # to be sure
foreach  $file (@ALL){
    last if $SPDR->CheckTime(-1);

    chomp($file);
    push(@TEMP,$file);

    # If soft-link, check if real file is present or not
    if ( -l $file ){
	if ( $DOSL ){
	    $realfile = readlink($file);
	    next if ( ! -e $realfile);
	} else {
	    next;
	}
    } else {
	$realfile = "";
    }

    # Add hook file which will globally leave
    if (( -e $ENV{HOME}."/$SELF.quit" ||  -e "$CHKDIR/$SELF.quit") && ! $DEBUG ){
	my($f)=((-e "$CHKDIR/$SELF.quit")?"$CHKDIR/$SELF.quit":$ENV{HOME}."/$SELF.quit");
	print $FO "Warning :  $f is present. Leaving\n";
	last;
    }

    # Skip some known pattern
    if ( $file =~ m/reco\/StarDb/){  next;}

    # We need to parse the information we can
    # save in the ddb
    $file =~ m/(.*\/)(.*)/;
    $path = $1; $file = $2;
    chop($path);
    $hpath= $path;

    if ($SUBPATH eq ""){
	$hpath=~ s/$SCAND/$HPSSD/;
    } else {
	$hpath=~ s/$SUBPATH//;
    }


    # Is a disk copy ??
    $fC->clear_context();
    if ( $path =~ m/\/star\/data/){
	$node    = "";
	$storage = "NFS";
	$fC->set_context("path = $path",
			 "filename = $file",
			 "storage = NFS",
			 "site = $SITE");

    } else {
	$storage = "local";
	$node    = $NODE;
	$fC->set_context("path = $path",
			 "filename = $file",
			 "storage = local",
			 "site = $SITE",
			 "node = $NODE");
    }
    @all1 = $fC->run_query("size");



    # HPSS copy (must be the last context to use clone_location() afterward )
    $fC->clear_context();
    $fC->set_context("path = $hpath",
		     "filename = $file",
		     "storage = HPSS",
		     "site = $SITE");
    @all = $fC->run_query_cache("size");



    if ($#all == -1){
	$unkn++;
	&Stream("Warning : File not found as storage=HPSS -- $path/$file \n\t[$hpath]\n\t[$file]");

    } else {
	$mess = "Found ".($#all+1)." records for [$file] ";

	if ( $realfile ne ""){
	    @stat   = stat($realfile);
	} else {
	    @stat   = stat("$path/$file");	    
	}
	
	if ( $#stat == -1 ){
	    &Stream("Error : stat () failed -- $path/$file");
	    next;
	}

	# if( $stat[7] != 0){
	#    $sanity = 1;
	# } else {
	#    $sanity = 0;
	# }


	if ($#all1 != -1){
	    $old++;
	    #print "$mess Already in ddb\n";

	} else {
	    #print "Cloning $hpath $file\n";
	    if ( ! $fC->clone_location() ){
		#print "Cloning of $file did not occur\n";

	    } else {
	        &Stream("$mess File cloned ".sprintf("%.2f %%",($new/($#ALL+1))*100))
		    if ($new % 10 == 0);
		$fC->set_context("persistent= 0");

		$fsize = $stat[7];
		@own   = getpwuid($stat[4]);
		$prot  = $SPDR->ShowPerms($stat[2]);

		# Enabled, it may update createtime / not enabled, it will likely
		# set to previous value in clone context - Ideally, do NOT restore.
		#$dt    = &UnixDate(scalar(localtime($stat[10])),"%Y%m%d%H%M%S");
		$fC->set_context("path       = $path",
				 "storage    = $storage",
				 "persistent = 0",
				 "size       = $fsize",
				 "owner      = $own[0]",
				 "protection = $prot",
				 # "createtime = $dt",
				 "available  = 1",
				 "site       = $SITE");
		if ( $node ne ""){
		    #print "Setting node to $node\n";
		    $fC->set_context("node       = $node",
				     "nodecomment= 'Added by $SELF'",
				     "pathcomment= 'Added by $SELF'");
		}

		$fC->debug_on() if ( $DEBUG );
		if ( ! $fC->insert_file_location() ){
		    &Stream("Error : Attempt to insert new location [$path] failed");
		    $failed++;
		} else {
		    $new++;
		}
		if ( $DEBUG ){
		    die "DEBUG mode, Quitting\n";
		}
	    }
	}


    }
}

# for statistics purposes
$ext = $SPDR->CheckTime(-3);

$fC->destroy();
$SPDR->ToFromCache(1,@TEMP);


FINAL_EXIT:
    if ($LOUT){
	print $FO
	    "$SELF :: Info : Summary for $SCAND/$SUB\n",
	    ($unkn  !=0 ? "\tUnknown = $unkn ".sprintf("%2.2f%%",100*$unkn/($unkn+$new+$old))."\n": ""),
	    ($old   !=0 ? "\tOld     = $old\n"   : ""),
	    ($new   !=0 ? "\tNew     = $new\n"   : ""),
	    ($failed!=0 ? "\tFailed  = $failed\n": ""),
	    "\tTimes   = $etimer / $ext\n";

	# Added 2009/09
	if ( $MARKERF ne ""){
	    if ( $new != 0 || $unkn  !=0 ){
		# create the marker file if set
		unlink($MARKERF) if ( -e $MARKERF);
		if ( ! open(FM,">$MARKERF")){
		    print $FO "$SELF :: Could not open $MARKERF\n";
		} else {
		    print FM "Auto-created - ".localtime()."\n";
		    close(FM);
		}
	    } else {
		# Nothing new ... but we would be safe to delete only in no cache
		# mode or cache but full pass
		if ( $DOCACHE ){
		    if ( $SPDR->ToFromCache(-2) == 0 ){
			unlink($MARKERF) if ( -e $MARKERF);
		    }
		} else {
		    # No cache, delete if exists as we checked a full list
		    unlink($MARKERF) if ( -e $MARKERF);
		}
	    }
	}


	# Check if we have opened a file
	if ($FO ne STDERR){
	    print $FO 
		"Scan done on ".localtime()."\n",
		"Time taken the operation ($SCAND) $ext\n";
	    close($FO);

	    # Save previous
	    if ( $failed!=0 || $unkn!=0 || $old!=0 || $new!=0 ){
		print "Have lines, summary done\n";
		if ( -e $FLNM.".last" ){  unlink($FLNM.".last");}
		if ( -e $FLNM ){          rename($FLNM,$FLNM.".last");}
		# rename new to final name
		if ( ! rename("$FLNM.tmp","$FLNM") ){
		    print "Failed to install $FLNM\n";
		}
	    } else {
		unlink("$FLNM.tmp") if ( -e "$FLNM.tmp");
		print "No need for a summary for $SCAND\n";

		# we need to remove old one too so we clean the record
		# as there is nothing to do with that one
		# BUT this can be done ONLY if we are not using caching
		if ($DOCACHE){
		    if ( open(FO,">$FLNM.tmp") ){
			print FO 
			    "Caching used - Pass done on ".localtime()." found no changes\n".
			    " - cache will expire in ".$SPDR->ToFromCache(-2)." passes\n";
			open(FI,"$FLNM");
			while ( defined($line = <FI>) ){  print FO "$line";}
			close(FI);
			close(FO);
			unlink("$FLNM");
			rename("$FLNM.tmp","$FLNM");
		    }
		} else {
		    unlink("$FLNM")     if ( -e "$FLNM");
		    $SPDR->ToFromCache(-1);
		}

	    }
	}
    } else {
	# if nothing was output, delete ALL files (especially if they
	# were old)
	if ($FO ne STDERR){
	    close($FO);
	    unlink("$FLNM.tmp") if ( -e "$FLNM.tmp");
	    unlink("$FLNM")     if ( -e "$FLNM" && 
				     ( $failed!=0 || $unkn!=0 || $old!=0 || $new!=0 ));
	}
    }

print "Done\n";



# Writes to file or STD and count lines
sub Stream
{
    my(@lines)=@_;

    foreach $line (@lines){
	$LOUT++;
	chomp($line);
	print $FO "$SELF :: $line\n";
    }
}


