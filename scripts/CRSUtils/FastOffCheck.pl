#!/opt/star/bin/perl -w

#
# This script checks if jobs are done or not.
# Done jobs are simply based on the appearance of
# the root files associated to the job.
# this script is meant to run in a cronjob ...
#
# There is NOTHING to change from this script.
# Use arguments like
#
# .../FastOffCheck.pl dev /star/data27/reco 12
#
# where
#  arg1 is the directory to scan
#  arg2 the path where the files are supposed to appear
#  arg3 a retention time for the output in days
#

use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use RunDAQ;

$LIB     = "dev";
$TARGET  = "/star/data19/reco";
$FIND    = "/usr/bin/find";
$UPDATE  = 0;
$RETENT  = 14;
$SSELF   = "FastOffCheck";
$PID     = sprintf("%5.5d",$$);
$DEBUG   = $ENV{FastOffCheck_DEBUG};

$LIB     = shift(@ARGV) if ( @ARGV );
$TARGET  = shift(@ARGV) if ( @ARGV );
$RETENT  = shift(@ARGV) if ( @ARGV );
$UPDATE  = shift(@ARGV) if ( @ARGV );   # 0, scan and delete if old,
                                        # 1, scan and enter in db
                                        # 2, get db entries and compare

exit if ( ! -d $TARGET);                # do not lose time, leave

# You can debug a specific run #
$DEBUGR    = $ENV{FastOffCheck_DEBUGR}||""; # 12058024; # 8348080;
$RMEVROOT  = 1==1;                          # could be a parameter later, delete event.root files

# Assume standard tree structure
$JOBDIR    = "/star/u/starreco/$LIB/requests/daq/archive/";
$SCRATCH   = (defined($ENV{SCRATCH})?$ENV{SCRATCH}:"/tmp/$<");
$SPACEPRGM = (defined($ENV{STAR_SCRIPTS})?$ENV{STAR_SCRIPTS}:"/afs/rhic.bnl.gov/star/packages/scripts")."/dfpanfs";


# create scratch if not there
if ( ! -d $SCRATCH){  mkdir($SCRATCH);}

$startT = "".localtime();
if ( $DEBUG ){
    print &Pid."Begin with ".($DEBUGR eq ""?"all runs":$DEBUG)."\n";
}

$lockf = $TARGET;
$lockf =~ s/\//_/g;
$lockf = $SCRATCH."/$lockf.lck";
if ( -e $lockf ){
    my(@info)=stat($lockf);
    my($delta)=time()-$info[10];
    my($DLIMIT)=14400; # 4 hours

    if ( $delta > $DLIMIT ){
	open(FI,$lockf); chomp($ligne = <FI>); close(FI);
	my($PPid,$tmp) = split(";",$ligne);
	if ( $PPid ne "" && $tmp ne ""){
	    # parsing is OK
	    kill 9,$PPid;
	    print &Pid."Killed previous $PPid and removing lock file $lockf ".localtime()."\n";
	} else {
	    print &Pid."Removing lock file $lockf (could not get previous PID) ".localtime()."\n";
	}

	rdaq_set_message($SSELF,"A lock file exist",
			 "Age reached ".(int($DLIMIT/6/6)/100)." hours - will remove and proceed");
	unlink($lockf);
    } else {
	print &Pid."Skipping - $lockf is present\n";
	rdaq_set_message($SSELF,"A lock file exist","Its age is ".
			 (int($delta/6/6)/100)." hours old (<$DLIMIT seconds) - leaving for now");
	exit;
    }
}
# else open a lock and delete later
open(FLCK,">$lockf") || die "Could not create $lockf\n";
print FLCK "$$;".localtime()."\n";
close(FLCK);




# Fault tolerant. No info if fails.
if( ! opendir(DIR,"$JOBDIR") ){
    print &Pid."$JOBDIR does not exists\n";
    exit;
} else {
    print &Pid."DEBUG $JOBDIR can be opened\n" if ($DEBUG);
    closedir(DIR);

}




print &Pid."DEBUG Update=$UPDATE\n" if ($DEBUG);

if ($UPDATE == 0){
    print &Pid."Scanning $JOBDIR vs $TARGET on ".localtime()."\n";

    if ( -e $SPACEPRGM ){
	print &Pid."DEBUG Getting space for $TARGET using $SPACEPRGM\n" if ($DEBUG);
	chomp($space = `$SPACEPRGM  $TARGET`);
	$space =~ m/(.* )(\d+)(%.*)/;
	$space =  $2;

	open(FO,">$TARGET/FreeSpace.new") || &Die("Could not open $TARGET/FreeSpace.new");
	print FO "$space\n"; close(FO);
	unlink("$TARGET/FreeSpace") if ( -e "$TARGET/FreeSpace");
	rename("$TARGET/FreeSpace.new","$TARGET/FreeSpace");

	print &Pid."DEBUG FreeSpace is $space\n" if ($DEBUG);
    }


    # The job directory itself needs to be scanned periodically
    # as some jobs escapes the scan - accumulation is then a burden
    # on later passes. But take it at a slightly large time than
    # what is passed on the cmd line
    my($t)=int($RETENT*1.5);
    @all = `$FIND $JOBDIR/ -maxdepth 1 -type f -mtime +$t`;
    foreach $el (@all){
	chomp($el);
	$dest =  $el;
	$dest =~ s/archive\//archive\/old\//;
	print &Pid."$el moved to old\n" if rename($el,$dest);
    }
    opendir(DIR,"$JOBDIR");

    #
    # read target disk in whole - cahe the full file list
    #
    chomp(@FILES = `cd $TARGET ; $FIND -type f -name *.MuDst.root`);
    foreach $file (@FILES){
	$loc =  $file;
	$loc =~ s/.*\///;
	$FILES{"$file"} = $loc;
    }
    undef(@FILES);


    $TAKEN = $COUNT = 0;

    # do the cleaning by comparing to the job directory - if un-available, this
    # script may not work.
    while( defined($jfile = readdir(DIR)) ){
	# print &Pid."$jfile\n";
	if( $jfile =~ /(.*_)(st_.*)/){
	    $tree = $1;
	    $file = $2;

	    # print &Pid."$jfile Tree=$tree file=$file\n";
	    next if ($file =~ /st_laser/);  # some knowledge of our file naming

	    if ($DEBUG){
		if ($file =~ m/$DEBUGR/){ print &Pid."DEBUG **** found it $file ***\n";}
	    }


	    $tree =~ s/_/\//g;
	    chop($tree);        # remove trailing '/'
	    if( -e "$JOBDIR/old/$jfile.checked"){
		@stat1 = stat("$JOBDIR/old/$jfile.checked");
		@stat2 = stat("$JOBDIR/$jfile");

                # can happen of disappear like a massive delete or move
		next if ($#stat2 == -1 || $#stat1 == -1);

		if ( $stat1[10] >= $stat2[10]){
		    if ($DEBUG){
			if ($file =~ m/$DEBUGR/){ 
			    print &Pid."DEBUG **** $jfile.checked exist [skip]\n";
			}
		    }
		    next;
		} else {
		    print &Pid."$jfile is more recent than last check. Rescan\n";
		    unlink("$JOBDIR/old/$jfile.checked");
		}
	    }

	    # double check the conformity of the job file name
	    if( $tree !~ m/$LIB/){
		print &Pid."WARNING :: Ill-formed $jfile found in $JOBDIR\n";
		push(@MOVE,$jfile);
	    } else {
		if ( ! -e "$SCRATCH/$file.done"){
		    if ($DEBUG){
			if ($file =~ m/$DEBUGR/){
			    print &Pid."DEBUG **** Did NOT find $SCRATCH/$file.done [treat]\n";
			}
		    }

		    $COUNT++;
		    if ($DEBUG){
			if ( $COUNT%50==0 ){
			    print &Pid."DEBUG Got $TAKEN/$COUNT $file\n";
			}
		    }

		    # print &Pid."Searching for $file\n";
		    chomp($lfile = `cd $TARGET ; $FIND -type f -name $file.MuDst.root`);
		    #if ( ! defined($lfile = $FILES{"$file.MuDst.root"}) ){  $lfile = "";}

		    if( $lfile ne ""){
			# found it so it is done - if not, it may be on another disk
			# .checked file is really what will disable the check though
			open(FF,">$SCRATCH/$file.done") || &Die("Cannot open $SCRATCH/$file.done");
			close(FF);

			@info = stat("$TARGET/$lfile");
			if ( $#info == -1){
			    print &Pid."Could not find $TARGET/$lfile\n";
			    next;
			} elsif  ( $info[7] == 0){
			    print &Pid."DEBUG 0 size file for $lfile (??)\n" if ($DEBUG);
			    next;
			}

			($tree,$el) = $lfile =~ m/(.*\/)(.*)/;
			chop($tree);
			$tree =~ s/\.\///;

			$TAKEN++;
			print &Pid."DEBUG $el --> $TARGET/$tree\n" if ($DEBUG);

			$LOCATIONS{"$file.daq"} = "$TARGET/$tree";
			#rdaq_set_message($SSELF,"New file found as done with prod","$file");
			push(@DONE,"$file.daq");
			push(@MOVE,$jfile);
		    } else {
			if ($DEBUG){
			    if ($file =~ m/$DEBUGR/){
				print &Pid."DEBUG **** Found $SCRATCH/$file.done or lfile is null [skip]\n";
			    }
			}

			# print &Pid."DEBUG Could not find $TARGET/$tree/$file.MuDst.root\n" if ($DEBUG);
		    }
		}
	    }
	}
    }
    closedir(DIR);


    if( ! -d "$JOBDIR/old"){  mkdir("$JOBDIR/old",0755);}

    # Also scan the main tree for obsolete files
    if( -d $TARGET){
	#print &Pid."Searching for f in $LIB from $TARGET\n";
	if ( -e "$TARGET/$LIB"){
	    chomp(@all = `cd $TARGET ; $FIND $LIB -type f -mtime +$RETENT`);
	} else {
	    push(@all,`cd $TARGET ; $FIND -type f -mtime +$RETENT`);
	    push(@all,`cd $TARGET ; $FIND -type f -empty`) if ( $^O =~ /linux/);
	    chomp(@all);

	    @all = grep(!/StarDb/,@all);
	}
	foreach $el (@all){
	    print &Pid."Deleting $TARGET/$el\n";
	    unlink("$TARGET/$el");
	    $el =~ s/.*\///g;
	    $el =~ s/\..*//;

	    # job file should be moved in old/ so the job directory
	    # does not get too large
	    if (-e "$JOBDIR/$el"){
		rename("$JOBDIR/$el","$JOBDIR/old/$el");
		print &Pid."Renaming $JOBDIR/$el to the old/ directory\n";
	    }

	    $el .= ".daq";

	    if( ! defined($LOCATIONS{$el}) ){
		$LOCATIONS{$el} = 0;
	    }
	}
    }


    $obj = rdaq_open_odatabase();
    if($obj){
	foreach $el (keys %LOCATIONS){
	    if( ! rdaq_set_location($obj,$LOCATIONS{$el},$el) ){
		print &Pid."Failed to set location for $el\n";
	    }
	}

	if ($#DONE != -1){
	    print &Pid."Setting files with status=2 if status=1 [".join(" ",@DONE)."]\n";
	    rdaq_toggle_debug(1);
	    @all = rdaq_set_files_where($obj,2,1,@DONE);
	    rdaq_close_odatabase($obj);

	    # reshaped to realistic success status
	    # between @DONE and @all, the difference could be files already entered
	    # but for which we had previously set status=2 ... or simply failed entries.
	    foreach $jfile (@all){
		rdaq_set_message($SSELF,"New file found as done with prod","$jfile");
		$SUCCESS{$jfile} = 1;
	    }

	    # rescan the DONE array and see what's missing - added 2015/03
	    foreach $jfile (@DONE){
		if ( ! defined($SUCCESS{$jfile}) ){
		    print &Pid."Error updating $jfile status from 1 to 2 occured\n";
		}
	    }

	    # if we have a problem here, not  abig deal (time stamp will
	    # be compared again)
	    foreach $jfile (@MOVE){
		# rename("$JOBDIR/$jfile","$JOBDIR/old/$jfile");
		open(FO,">$JOBDIR/old/$jfile.checked");
		print FO "$0 ".localtime()."\n";
		close(FO);
	    }
	}
    }


} elsif ($UPDATE == 1) {
    # Scan the directory for all files present and mark their
    # path in the database. This is rarely used. And done
    # only to update the database with a new location
    # directory if files are moved ...
    $obj = rdaq_open_odatabase();
    if($obj){
	chomp(@all = `cd $TARGET ; $FIND -type f -name '*.MuDst.root'`);
	print &Pid."We found $#all in the db\n" if ($DEBUG);
	foreach $el (@all){
	    $el =~ m/(.*\/)(.*)/;
	    ($tree,$el) = ($1,$2);
	    $el =~ s/\..*//;
	    $el .= ".daq";

	    chop($tree);
	    if( ! defined($LOCATIONS{$el}) ){
		$LOCATIONS{$el} = "$TARGET/$tree";
	    }
	}

	foreach $el (keys %LOCATIONS){
	    rdaq_set_location($obj,$LOCATIONS{$el},$el);
	}
	rdaq_close_odatabase($obj);
    }

} else {
    print &Pid."Update is $UPDATE i.e. default\n";
    $obj = rdaq_open_odatabase();

    #rdaq_toggle_debug(1);
    @all = rdaq_get_files($obj,2,0);

    foreach $file (@all){
	if ( $path  = rdaq_get_location($obj,$file) ){
	    print &Pid."Checking $path for $file\n";
	    $qfile = $ffile = $file;
	    $ffile =~ s/\.daq/\.MuDst\.root/;
	    $qfile =~ s/\.daq/\.hist\.root/;
	    $efile =~ s/\.daq/\.event\.root/;

	    if ( ! -e "$path/$ffile"){
		# file is not found, somehting is not right
		rdaq_set_location($obj,0,$file);
		print &Pid."$path $ffile not found\n";
	    } else {
		# delete only if MuDST is found
		unlink ($efile) if ( -e "$path/$efile" && $RMEVROOT );

		foreach $tfile (("$path/$ffile","$path/$qfile")){
		    # we will require for this to have both event and hist
		    # present or disable it
		    if ( -e $tfile){
			@info = stat($tfile);
			if ( $info[7] == 0){
			    # disable those records
			    print &Pid."Bogus zero size file found $tfile\n";
			    rdaq_set_message($SSELF,"Bogus zero file size found","$tfile");
			    rdaq_set_location($obj,0,$file);
			} else {
			    #if ( $file =~ /794028/){
			    #print &Pid."Found $file\n";
			    #}
			}
		    }
		}
	    }
	}
    }

}

DONE:
    if ( $DEBUG ){  print &Pid."and we are ending started @ $startT\n";}
    &end();




sub end {  unlink($lockf) if ( -e $lockf);    }


sub Die
{
    my(@MSG)=@_;
    my($mess);

    &end();

    foreach $mess (@MSG){
	chomp($mess);
	die "$mess\n";
    }
}


sub Pid
{
    return $PID." ".localtime()." ";
}
