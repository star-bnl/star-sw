#!/opt/star/bin/perl -w


#
# This script will function as a loop and will fill
# the operation DAQInfo table.
# BEWARE !!! This is an infite loop process filling
# information in the DAQInfo table.
#

use lib "/afs/rhic.bnl.gov/star/packages/scripts/";
use RunDAQ;

# Mode 1 will quit
$mode    = 1;     # default mode - overwritten by argument 1
$sltime  = 60;    # sleep time between default between loops - overwriten by arguments 2
$FILE    = "";    # Default file name for LOG - overwirten by argument 3 (i.e. DAQFill.log);
$EXPIRET = 172800;# 172800 = 48 hours / 21600 = 6 hours / 14400 = 4 hours / 10800 = 3 hours delays expected

$SSELF   = "DAQFill";


$mode   = shift(@ARGV) if ( @ARGV );
$arg1   = shift(@ARGV) if ( @ARGV );
$FILE   = shift(@ARGV) if ( @ARGV );

# We added this in 2004 for a bootstrap by run number
# range.
if ( defined($arg1) ){
    if ( $arg1 > 0){ $sltime = $arg1;}
} else {
    $arg1 = 0;
}

if ( defined($ENV{FO_DEBUG}) ){
    rdaq_toggle_debug();
}

# We add an infinit loop around so the table will be filled
# as we go.
&Print("$0 starting on ".localtime()." ($sltime)\n");
rdaq_set_message($SSELF,"Starting","DAQInfo filler for FastOffline has started");
do {
    $ctime = localtime();
    $dbObj = rdaq_open_odatabase();
    if( $dbObj){
	#&Print("O-Database opened\n");
	if ( ($obj = rdaq_open_rdatabase()) == 0){
	    &Print("Failed to open R-Database on ".localtime()."\n");
	    rdaq_set_message($SSELF,"Failed to open R-Database",
			     "This will prevent fetching of new records");
	} else {
	    # &Print("R-Database opened\n");

	    # get the top run. Note that <= 0 would get all
	    # runs ever entered in this database.
	    if ($arg1 < 0){
		$run  = - $arg1 . ".0";
		$crun = $prun= "0.0";
	    } else {
		$run = $mode*rdaq_last_run($dbObj);
		($prun,$crun)= &GetRun();
	    }

	    if($prun ne $run){
		# We need to frequently re-issue the run number message to be sure
		# to get it later at the end of the log.

		# DO NOT change this next message - it is used by GetRun()
		&Print("Last Run=$run on ".localtime()."\n");

		# the message format do not matter afterward
		if ( $run < 0){
		    rdaq_set_message($SSELF,
				     "Last run in our log",(-$run)." but asked to bootstrap all records from the beginning");
		} elsif ($run == 0){
		    rdaq_set_message($SSELF,"Full scan","We will update or get new runs >= 0");
		} else {
		    if ( $run ne $crun){
			# avoid too frequent streamed messages
			rdaq_set_message($SSELF,"Last run in our log",
					 "We will update or get new runs >= $prun (now at $run)");
		    }
		}
	    }

	    $tot   = 0;
	    $begin = 0;
	    $by    = 1000;
	    if($run >= 0){
		# fetch new records since prun run number (be carefull of the change of
		# logic between prun and run)
		my($count,$N);

		&Print("Bootstrap case, run = $run ( >= 0) ; prun = $prun\n");
		$count = -1;
		rdaq_set_dlevel(1);

		# we use prun instead on run but do not want to use 0.0
		if ( $prun eq "0.0" || $run == 0){
		    $ctime = localtime();
		    $prun  = $run;
		}

		do {
		    $count++;
		    # cleanup
		    undef(@records);

		    @records = rdaq_raw_files($obj,$prun,"$begin,$by");

		    # display info
		    if ($#records != -1){
			&Print("Fetched ".($#records+1)." records on $ctime, ");

			# record entries
			$N = rdaq_add_entries($dbObj,@records);
			&Print("Adding $N entr".($N==1?"y":"ies").($tot==0?"\n":" to $tot so far\n"));
			$tot += $N;
		    }
		    $begin += $by;
		} while ($#records != -1);
		&Print("Got $tot records in $count pass".($count==1?"":"es")."\n");
		rdaq_set_message($SSELF,"Fetched new records",
				 "$tot records in $count pass".($count==1?"":"es")) if ($tot != 0);

	    } else {
		&Print("Checking entries on ".localtime()."\n");
		$mode    = 0;  # main loop reset
		do {
		    @records = rdaq_raw_files($obj,$run,"$begin,$by");

		    &Print("Updating ".($#records+1)." on ".localtime()."\n");
		    $tot   += rdaq_update_entries($dbObj,@records);
		    $begin += $by;
		} while ($#records != -1);
		rdaq_set_message($SSELF,"Bootstrap","Updated or inserted $tot records") if ($tot != 0);
	    }


	    # close
	    rdaq_close_rdatabase($obj);

	}
	rdaq_close_odatabase($dbObj);
    } else {
	&Print("Failed to open O-Database on ".localtime()."\n");
    }

    sleep($sltime*$mode);
} while(1*$mode );


#
# perform IO on a file or STDOUT.
#
sub Print
{
    my(@all)=@_;

    if($FILE ne ""){
	while( ! open(FO,">>$FILE")){;}
	$FO = FO;
    } else {
	$FO = STDOUT;
    }
    foreach $el (@all){
	print $FO "$SSELF :: ".localtime()." : $el";
    }
    if($FO ne STDOUT ){ close($FO);}
}

#
# Get last recorded run in file
#
sub GetRun
{
    my(@lines,$line,$rv,$cv);
    my(@info,$delta,$lrv);

    $cv = $rv = "0.0";
    if( $FILE ne ""){
	@lines = `/usr/bin/tail -20 $FILE`;
	foreach $line (@lines){
	    if ($line =~ m/(Run=)(\d+)\.(\d+)/){
		# this run information comes from rdaq_last_run() which will look
		# at the latest record in the db. We need to fetch records since an
		# older run number until now.
		$cv = $rv = "$2.$3";
	    }
	}
	if ( $rv ne "0.0" ){
	    if ( -e "/tmp/$SSELF.last_run" ){
		open(FI,"/tmp/$SSELF.last_run");
		chomp($lrv = <FI>);
		close(FI);

		@info  = stat("/tmp/$SSELF.last_run");
		$delta = time()-$info[10];
		if ( $delta > $EXPIRET ){
		    my($irv)=int($lrv+0.75*($rv-$lrv)); # move only part of the way

		    rdaq_set_message($SSELF,"Expired",
				     "RunNumber expiration for $lrv - now $rv, moving to $irv");
		    &Print("Expiration time has arrived for $lrv -  now $rv, moving to $irv\n");
		    open(FO,">/tmp/$SSELF.last_run");
		    print FO "$rv\n";
		    close(FO);
		} else {
		    # overwrite
		    if ($lrv ne "0.0"){
			&Print("Using older run $lrv instead of $rv ($delta <= $EXPIRET)\n");
			$rv = $lrv;
		    }
		}
	    } else {
		# create one - first time this runs or parsin did not find the magic
		# line over the past 10 lines
		open(FO,">/tmp/$SSELF.last_run");
		print FO "$rv\n";
		close(FO);
	    }
	}
    }

    ($rv,$cv);
}

