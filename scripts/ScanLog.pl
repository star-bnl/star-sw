#!/opt/star/bin/perl -w
#ScanLog2.pl

#
# Version2
#
# This script was written by Nikita Soldatov, July 2001.
# Its purpose is to scan a log directory, scan for errors
# and fill a database (operation -> RJobInfo) with all the
# required information. This information is viewable via
# a cgi also developped by your kind servant, Nikita ... :)
#
# Unique argument is a production tage. This script is
# suitable for a crontab job.
#
# Usage :
#   % 'ThisScriptName' [productionTag] [Flag] [Kind]
#
# productionTag -> default P01he
# Flag             if specified as 'delete' empty the database first
#                  (maintainance only).
# Kind             default "daq". Sets the path to scan and where
#                  to look. See $log_dir etc ...
#
#
# Added zcat support. Need tuning (twice a zcat | tail is
# inefficient).
#
#

use strict;
use DBI;
#use File::stat;


my $ProdTag = defined($ARGV[0])?$ARGV[0]:"P01he";
my $Kind    = defined($ARGV[2])?$ARGV[2]:"daq";

&CheckLockFile();

my $min_size = 1200;   # minimal size for a log file in bytes (only error file scanned if smaller)
my $min_time = 60;     # minimal age of a log before any treatement at all
my $max_time = 3600;   # age of log a necessary before the creation of a marker file
my $max_file = 250000; # PANASAS 350000 max

#dirs
my $log_dir  = "/gpfs01/star/rcf/prodlog/$ProdTag/log/$Kind/";
my $job_dir  = "/star/u/starreco/$ProdTag/requests/$Kind/jobfiles/";
my $arch_dir = "/star/u/starreco/$ProdTag/requests/$Kind/archive/";

my $datasourse = "DBI:mysql:operation:duvall.star.bnl.gov";
my $username = "starreco";
my $SDIR     = "ScanLog";
my $SELF     = "$SDIR :: ".localtime()." : ";

my $logname;
my $shortname;
my $fsize;
my $deltatime;
my $mtime;
my $ctime;
my $job_name;
my $id;
my $XTrigger;
my $err_file;
my $Status;
my @log_errs1;
my @log_errs2;
my @job_errs;
my $logerr;
my $err;
my $cmprss;
my $cmd;

my $i = 0;
my $m_time;
my $c_time=1;
my $pmtime;
my $node = "unknown";

my @fc;

my $dbh1 = DBI->connect($datasourse,$username)
    or &Die("Can't connect to $datasourse: $DBI::errstr\n");

my $del;

if( defined($ARGV[1]) ){
    # An option ?
    $dbh1->do("DELETE FROM RJobInfo") if ($ARGV[1] eq "delete");
}

my $sth1 = $dbh1->prepare("INSERT INTO RJobInfo ".
			  "(ProdTag, XTrigger, LFName, ".
			  "ctime, mtime, node, ErrorStr) ".
			  "VALUES (?, ?, ?, ?, ?, ?, ?)");

my $sth3=$dbh1->prepare("SELECT id, mtime FROM RJobInfo ".
			"WHERE ProdTag = \"$ProdTag\" AND ".
			"XTrigger = ? AND ".
			"LFName = ?");

my $sth4 = $dbh1->prepare("UPDATE RJobInfo SET ".
			  "mtime=?, ".
			  "node=?".
			  "ErrorStr=?, ".
			  "Status = 0 ".
			  "WHERE id=?");

my $DEBUG=$ENV{ScanLogDEBUG};
my $count = 0;


if( ! $sth3 || ! $sth1){
    &Die("Problem !! Cannot prepare statement\n");
}

my(%JNAMES);
print "$SELF Opening archive directory $arch_dir\n" if ($DEBUG);
opendir(ARCH,"$arch_dir") || &Die("Could not open archive directory $arch_dir\n");
while ( defined($job_name = readdir(ARCH)) ){
  if($job_name !~ /st_/ && $job_name !~ /rcf.*evts/ ){ next;}
  if ($job_name =~ m/(.*)(st_.*)/){
      $JNAMES{$2} = $job_name;
  } elsif ($job_name =~ m/(rcf.*evts)(.*)/){
      $JNAMES{$2} = $job_name;
  }
}
closedir(ARCH);


print "$SELF Opening log directory $log_dir\n" if ($DEBUG);
opendir(LOGDIR,$log_dir) || &Die("can't open $log_dir\n: $!");
if ( ! -d "$log_dir/$SDIR"){   mkdir("$log_dir/$SDIR",0755);}

while ( defined($logname = readdir(LOGDIR)) ){
    # we must check for only what we need since the directory
    # may contain other files. 
    $count++;
    if ( $count > $max_file){
	print "$SELF $log_dir contains more than $max_file . Please clean.\n" if ($DEBUG);
	&Die("$log_dir contains more than $max_file . Please clean.");
    }

    print "$SELF Looking at $logname\n" if ($DEBUG);

    if ( $logname =~ /\.log$/ || $logname =~ /\.log\.gz$/) {
	$shortname = $logname;
        $err_file = $logname  = $log_dir . $logname;
	$err_file =~ s/\.log/\.err/;

	# tried the un-compressed. If still nothing, well ...
	if( ! -e $err_file){  $err_file =~ s/\.gz// ;}

	# New CRS leaves .log .err while we have .log.gz .err.gz
	if ( $logname =~ m/\.gz/ &&  $err_file =~ m/\.gz/ ){
	    my($slog,$serr)=($logname,$err_file);
	    my($l);
	    $slog =~ s/\.gz//;
	    $serr =~ s/\.gz//;
	    if ( -e $slog && -e $slog){
		$l = 0; while ( ! unlink($slog) || $l == 10){ $l++; sleep(2);}
		if ($l == 10){ print "$SELF Warning: tried unlink($slog) 10 times and did not succeed\n";}

		$l = 0; while ( ! unlink($serr) || $l == 10){ $l++; sleep(2);}
		if ($l == 10){ print "$SELF Warning: tried unlink($serr) 10 times and did not succeed\n";}

		print "$SELF Found .log/.err while analyzing .log.gz / .err.gz for $shortname\n" ;
	    }
	}


	print "$SELF  --> stat($logname)\n" if ($DEBUG);
        @fc = stat($logname);
	if( $#fc != -1){
	    $deltatime = time() - $fc[9];
	} else {
	    # Note that a stat() may fail
	    $deltatime = 0;
	}

	# laps time has to be at minimum min_time
	if($deltatime > $min_time ){
	    # Take care of compression side effect i.e., the first
	    # time around, the .parsed file is .log.parsed while
	    # after compression, the file will be .log.gz.parsed
	    # We do not know a-priori what it will be ... So,
	    # delete the old one.
	    my $tmpname = $shortname;
	    $tmpname =~ s/\.gz//;
	    if( $tmpname ne $shortname){
		if( -e "$log_dir/$SDIR/$tmpname.parsed"){
		    print
			"Deleting (compressed version exists) ",
			"$tmpname.parsed\n";
		    unlink("$log_dir/$SDIR/$tmpname.parsed");
		}
	    }

	    if ( -e "$log_dir/$SDIR/$shortname.parsed"){
		# if a .parsed file exists, then skip it UNLESS
		# the log file is more recent than the .parsed file.
		my @info = stat("$log_dir/$SDIR/$shortname.parsed");
		if( $#info != -1){
		    $pmtime = $info[9];
		} else {
		    $pmtime = 0;
		}
		if ( $pmtime > $fc[9] ){
		    next;
		} else {
		    print "$SELF   --> Deleting $log_dir/$SDIR/$shortname.parsed\n" if ($DEBUG);
		    unlink("$log_dir/$SDIR/$shortname.parsed");
		}
	     } elsif ( $deltatime > $max_time ){
		 # after max_time, and only after, we create a .parsed
		 # file has a mark that we do NOT want to go through
		 # this log file again. However, the logic is such that
		 # if a run is started again, the .parsed file would be
		 # deleted and the related log file would be treated as
		 # a new one.
		 print "$SELF   --> Creating $log_dir/$SDIR/$shortname.parsed\n"    if ($DEBUG);
		 if ( open(FO,">$log_dir/$SDIR/$shortname.parsed") ){
		     print FO "$0 (Nikita Man) ".localtime()."\n";
		     close(FO);
		     chmod(0775,"$log_dir/$SDIR/$shortname.parsed");
		 }

	     }



	    # search for a file with similar name
	    $shortname =~ s/\.log.*//;
	    if( ! defined($job_name = $JNAMES{$shortname}) ){ next;}

	    # now, we have a job file
	    define_trigger($logname, $job_name);

	    if ( $fc[7] <= $min_size ){
		if($DEBUG){
		    print
			"Kind       : 1",
			"Found log  : $logname\n",
			"Error file : $err_file\n",
			"Short name : $shortname\n",
			"Deltatime  : $deltatime\n",
			"Size       : $fc[7]\n",
			"Errors in ERR file --> \n";
		}
		$err="";
		$cmd = &ZSHandle($err_file,"/usr/bin/tail -4");
		print "$SELF   --> Will execute [$cmd]\n" if ($DEBUG);
		@job_errs = `$cmd`;
		for ( $i=0;$i<=$#job_errs;$i++ ){
		    unless ( $err=~/$job_errs[$i]/ ){
			$err .= "$job_errs[$i] | ";
		    }
		}
		if($DEBUG){
		    print "$SELF $err\n";
		} else {
		    if($err ne ""){ print "$SELF error type 1 in $logname\n";}
		}
	    } else {
		my $tmp="";

		if($DEBUG){
		    print
			"$SELF\n",
			"\tKind       : 2",
			"\tFound log  : $logname\n",
			"\tError file : $err_file\n",
			"\tShort name : $shortname\n",
			"\tDeltatime  : $deltatime\n",
			"\tSize       : $fc[7]\n",
			"\tErrors in LOG & ERR files --> \n";
		}

		$err="";
		$node="unknown";

		# Get node name from header. Any header info should be around here
		$cmd = &ZSHandle($logname,"/usr/bin/head -5");
		#print "debug [$cmd] | grep 'running on'\n";
		# head uses SIGPIPE. Prevent STDERR polution.
		&DupSTDERR;
		print "$SELF --> Will execute [$cmd | grep 'running on']\n" if ($DEBUG);
		$node= `$cmd | grep 'running on'`;
		&ResSTDERR; 
		if ( $node =~ m/running/){
		    chomp($node);
		    $node=~ s/.*running on//i; 
		    $node=~ s/^\s*(.*?)\s*$/$1/;
		} else {
		    $node = "unknown";
		}
		#die "debug [$node]\n";
		

		# Get list of errors
		$cmd = &ZSHandle($logname,"/usr/bin/tail -5000");
		# the above grep is not full-path-ed because it uses 
		# -E (GNU grep)
		print "$SELF   --> Will execute $cmd | grep -E ...\n" if ($DEBUG);
		@log_errs2 = 
		    `$cmd | grep -E 'Break|Abort|Assert|relocation error'`;
		foreach $logerr (@log_errs2){
		    print "$SELF $logerr\n";
		    if ( $logerr=~/(\*+\s+Break\s+\*+)(.*)/ ){
			unless ( $err=~/$2/ ){
			    $err.=" $2 |";
			}
		    } elsif ( $logerr =~ m/^Abort/){
			unless ( $err =~ /Code was aborted/){
			    $err .= "Code was aborted | ";
			}
		    } elsif ( $logerr =~ m/(^CheckFail:+)(.*)/){
			# St_db_Maker.cxx >> CheckFail:: Assert will fail for Table
			# StSpaceChargeEbyEMaker.cxx >> CheckFail: Break of SpaceCharge performance
			unless ($err =~ /$2/){
			    $err .= "$2 | ";
			}
		    } elsif ( $logerr =~ m/(relocation error)(.*)/) {
			unless ($err =~ /$2/){
			    $err .= "$logerr | ";
			}
		    } else {
			unless ( index($err,$logerr) >= 0){
			    $err .= "$logerr | ";
			}
		    }
		}
		undef(@log_errs2);

		# Get event number - Faster to spilt in two loops
		$cmd = &ZSHandle($logname,"/usr/bin/tail -5000");
		print "$SELF   --> Will execute $cmd | /bin/grep 'Done with Event'\n" if ($DEBUG);
		@log_errs2 = `$cmd | /bin/grep 'Done with Event'`;
		foreach $logerr (@log_errs2){
		    if($logerr =~ m/(\d+)(\/run)/){
			$tmp = $1;
		    }
		}
		if($tmp ne "" && $err ne ""){
		    $err = "After $tmp events $err\n";
		}
		undef(@log_errs2);


		$cmd = &ZSHandle($err_file,"/usr/bin/tail -5");
		print "$SELF   --> Will execute $cmd\n" if ($DEBUG);
		@log_errs1 = `$cmd`;
		foreach $logerr (@log_errs1){
		    chomp($logerr);
		    print "$SELF Checking line [$logerr]\n" if ($DEBUG);
		    &define_err("Assertion.* failed",$logerr);
		    &define_err("Unexpected EOF",$logerr);
		    &define_err("Fatal in <operator delete>",$logerr);
		    &define_err("Fatal in <operator new>",$logerr);
		    &define_err("error in loading shared libraries",$logerr);
		    &define_err("Broken pipe",$logerr);
		}
		chop($err);

		if($DEBUG){
		    print "$SELF $err\n";
		} else {
		    if($err ne ""){ print "$SELF error type 2 [$err] in $logname\n";}
		}
	    } #else fsize/minsize compare

	    if ( $err ){
		$sth3->execute($XTrigger, $shortname)
		    or &Die("cannot execute sth3\n");
		#print $sth3->fetchrow_array()."\n";
		if ( ($id, $mtime) = $sth3->fetchrow_array() ){
		    print
			"old mtime : $mtime\n",
			"new mtime : $fc[9]\n";
		    if ( $mtime != $fc[9] ){
			#update record
			print "$SELF Updated $shortname\n";
			$sth4->execute($fc[9],$node,$err,$id);
		    }
		} else {
		    #insert record
		    print "$SELF Inserted $shortname\n";
		    $sth1->execute($ProdTag, $XTrigger, $shortname, $c_time, $fc[9], $node ,$err);
		}
		print "$SELF ----\n";
	    } #if $err
	    if ($DEBUG){ print "\n==============================\n";}
	} #if modtime/min_time
    } #if logname
} #while
closedir(LOGDIR);

# terminate statements handler
$sth1->finish();
$sth3->finish();
$sth4->finish();


# This commented block alows you to see a content of the table RJobInfo.
# If you want to see a content of the table every time you run the script
# uncomment this block.

#my $sth2 = $dbh1->prepare("SELECT id, ProdTag, XTrigger, LFName, ctime, mtime, Status, ErrorStr FROM RJobInfo");
#$sth2->execute();
#while (($id, $ProdTag, $XTrigger, $logname, $c_time, $m_time, $Status, $err) = $sth2->fetchrow_array()) {
#    print "$id  $ProdTag  $XTrigger  $logname  $Status  $m_time  $err\n";
#}
#$sth2->finish();


$dbh1->disconnect();
&DeleLockFile();



#subs
#=======================================



#
# This matches job generation script
#
sub define_trigger
{
    my ($lname,$jname) = @_;
    my (@temp);

    # define XTrigger, use a default value
    $XTrigger = "unknown";
    @temp = split(/_/, $jname);
    if ( $temp[0] !~ /^\d+/){ $XTrigger = $temp[0];}
    return $XTrigger;
}

#
# This was used post 2002
#
sub define_trigger_old {
    my ($lname,$jname) = @_;
    my @temp;
    my $i = 0;

    # define XTrigger, use a default value
    $XTrigger = "unknown";
    @temp = split(/_/, $jname);

    if($#temp == -1){ return;}
    print "$SELF Looking at $jname\n";

    # We assume that the trigger name will be after the Prodtag
    # appearance in the file name.
    while( $temp[$i++] ne $ProdTag ) {
	if($i == $#temp){ last;}
    }

    if ( substr($temp[$i],0,1) eq "2" || $i == ($#temp-1)){
	# Wrong field. XTrigger is missing and we
	# grabbed the next item = date.
	return;
    } else {
	$XTrigger = $temp[$i];
    }
}

#=======================================

sub define_err
{
    my ($errname,$logerr) = @_;

    #if( $errname =~ /Assertion/i){ print "[$errname][$logerr]\n";}
    if( $logerr =~ m/$errname/ ){
	#chomp($logerr);
	$err .= " $logerr |";
	print "$SELF $err";
    }
}

#
# handle Z-stuff and return the aproriate shell command
#
sub ZSHandle
{
    my($file,$shell)=@_;

    if($file =~ /\.gz/){
	"/bin/zcat $file | $shell";
    } else {
	"$shell $file";
    }
}


sub Die
{
    my($messg)=@_;
    &DeleLockFile();
    die $messg;
}


sub DeleLockFile
{
    my ($fllock)="/tmp/ScanLog$ProdTag.lock";
    if ( -e $fllock){  unlink($fllock);}
}

# Checks and creates
sub CheckLockFile
{
    # Checks if another one is running by using a lock file trick
    my ($fllock)="/tmp/ScanLog$ProdTag.lock";
    if ( -e $fllock){
	my($mtime)=86400;      # 24 hours
	my(@info)=stat($fllock);
	if ( (time()-$info[9] ) > $mtime){
	    # 3 hours
	    if ( unlink($fllock) ){
		print "$SELF $fllock was older than $mtime secondes. Removed on ".localtime()."\n";
	    } else {
		print "$SELF $fllock is older than $mtime secondes. Cannot remove on ".localtime()."\n";
	    }
	} else {
	    print "$SELF Found $fllock on ".localtime().". Ignored in [$$]\n";
	}
	exit;
    }
    open(FO,">$fllock");
    print FO localtime()."\n";
    close(FO);
}


# Dup & Restore
sub DupSTDERR
{
    open(SAVEERR,">&STDERR");
    open(STDERR,">/dev/null");
    select(STDERR);
}
sub ResSTDERR
{
    close(STDERR);
    open(STDERR,">&SAVEERR");
    close(SAVEERR);
}
