#
# This module is used to get information about the CRS queue
# system. Only a few routine implemented so far.
#
#

use Carp;

package CRSQueues;
require 5.000;
require Exporter;
@ISA = qw(Exporter);

@EXPORT= qw(CRSQ_submit
	    CRSQ_getcnt
	    CRSQ_check
	    CRSQ_Offset
	    CRSQ_Norm
	    );

$BMODE=1;
if ( $BMODE==0 ){
    # Those are OK for the OLD system, pre-2005
    $STATUS="/usr/local/bin/crs_status.pl -m";
    $JOBINF="/usr/local/bin/crs_status.pl -c";
    $JOBLST="/usr/local/bin/crs_status.pl -c";
    $QUEUEJ="";
    $SUBMIT="/usr/local/bin/crs_submit.pl";


    $CRSQ::MAXQ=6;       # support up to that number of queues
    $CRSQ::PFACT=5;      # some number of files arbitrary proportion factor
    $CRSQ::OFFSYNC=30;   # queue submission will continue up to this %tage
                         # of off-sync.

#} elsif ( $BMODE == 1) {
    # Condor-based
#    $STATUS="/home/starreco/newcrs/bin/crs_job -show_queues";
#    $JOBINF="/home/starreco/newcrs/bin/crs_job -show_crs_jobs_per_queue";
#    $JOBLST="/home/starreco/newcrs/bin/crs_job -stat_show_machines";
#    $QUEUEJ="/home/starreco/newcrs/bin/crs_job -create";
#    $SUBMIT="/home/starreco/newcrs/bin/crs_job -submit";

#    $CRSQ::MAXQ=6;       # support up to that number of queues
#    $CRSQ::PFACT=5;      # some number of files arbitrary proportion factor
#    $CRSQ::OFFSYNC=30;   # queue submission will continue up to this %tage
                         # of off-sync.

} elsif ( $BMODE == 1) {
    # Condor-based
    $JOBLST="/usr/bin/crs_job -machines";
    $STATUS="/usr/bin/crs_job -stat";
    $QUEUEJ="/usr/bin/crs_job -create";
    $SUBMIT="/usr/bin/crs_job -submit";

    $CRSQ::MAXQ=6;       # support up to that number of queues
    $CRSQ::PFACT=5;      # some number of files arbitrary proportion factor
    $CRSQ::OFFSYNC=30;   # queue submission will continue up to this %tage
                         # of off-sync.

} else {
    # If don't know, die
    die "No batch mode. Sorry.\n";

}


$CRSQ::GETCNT=0;                # called the getcnt() routine (not a config var)
$CRSQ::fcdrop=0;
$CRSQ::fldrop=1;

# %CRSQ::TOT     assoc-array of queue info (total slots)
# %CRSQ::RUN     assoc-array of running jobs pass1
# %CRSQ::FND     assoc-array of found jobs pass2
# %CRSQ::DIF     hold initial differences (available slots)
# %CRSQ::WARN    Avoid multiple warnings by flagging it

#
# This routine checks the job files matching $pat
# if they are anywhere in the queue. If not, the jobfiles
# found are moved to a directory named $mdir. Note that
# this function will perform its expected task only when
# the queue will contain something ...
#
sub CRSQ_check
{
    my($pat,$mdir)=@_;
    my($line,$jfile);
    my(@result,@JOBS);
    my($tmp);


    @result = `$JOBLST`;

    # If the following test is true, something is wrong or we are at
    # the end of the year period or we are no longer running ...
    # Most probably, something is wrong with the CRS nodes (or at
    # least, we need to prevent it from happening again).
    if($#result == -1){ return;}

    if ( $BMODE == 0){
	# Get a list of jobs still in the queue
	foreach $line (@result){
	    chomp($line);
	    $job = (split("%",$line))[1];
	    $JOBS{$job} = 1;
	}
    } elsif ( $BMODE == 1 ){
	foreach $line (@result){
	    chomp($line);
	    $line =~ s/\s+/ /g;
	    $job  = (split(" ",$line))[0];
	    # also has that funky additional Id
	    @items= split("-",$job); pop(@items);
	    $job  = join("-",@items);
	    $JOBS{$job} = 1;
	    #print "$job\n";
	}
    }

    # Test if we measure something ; do not do anything if not even
    # a single job was found (this may be a bad sign by itself)
    @temp = keys %JOBS;
    if ($#temp == -1){  return;}


    # Scan the current directory for all job files
    @all = &Glob($pat);

    foreach $jfile (@all){
	if( ! defined($JOBS{$jfile}) ){
	    # It is no longer in the queue. We actually cannot much more
	    # here than moving the file into the archive directory.
	    if ($jfile =~ m/(.*\/)(.*)/){
		# path was specified in pattern
		$tmp =  $2;
	    } else {
		# no path specified
		$tmp = $jfile;
	    }
	    print "CRSQ :: Job $tmp no longer in the queue on ".
		localtime()."\n";
	    rename($jfile,"$mdir/$tmp");
	}
    }
}




# Check the number of slots in queue $qnum.
#
#  If $drop is specified, check all queues lt than $qnum but no less than
# $qnum - $drop.
#   drop >=0    by default, note that this routine calculates for MAX jobs for queue $qnum
#               and MAX-1 for lower queues.
#   $drop > 100 (unlikely number), all queues will be calculated as MAX
#   $drop < 0   $MAX-1 will enter in the base calculation of the number
#               of available slots for $qnum.
#   $drop < 100  $qnum will have MAX-1 and lower queues MAX
#
#
#  If $pat is specified, check files with that wildcard pattern. If plenty
# exists, return an error. See comments for this obscure mode ...
#  The input file parsing can be disabled by using $nchk parameter (in case the
# job files are pre-generated, we do not want to do this check).
#
# Returns the total number of available slots.
#
sub CRSQ_getcnt
{
    my($qnum,$drop,$pat,$nchk)=@_;

    my($i,$line,$ratio);
    my(@result,@items);
    my($NOTA,$TOT,$DEBUG);
    my(%NODES);


    $DEBUG = 0;

    # Sanity check
    if($qnum >= $CRSQ::MAXQ){
	return &Exceed("QueueNum",$CRSQ::MAXQ);
    }

    # Initialization
    if ( ! defined($drop) ){ $drop = 0;}
    $drop = &CRSQ_Norm($drop,1);

    $CRSQ::GETCNT = 1;
    $NOTA = 0;
    for($i=0 ; $i < $CRSQ::MAXQ ; $i++){
	$CRSQ::FND{$i} = $CRSQ::RUN{$i} = $CRSQ::TOT{$i} = 0;
	if( ! defined($CRSQ::WARN{$i}) ){ $CRSQ::WARN{$i} = 1;}
    }

    my($tmp)=(split(" ",$STATUS))[0];
    if ( ! -e $tmp){
	print "CRSQ :: Could not find $tmp necessary to proceed\n";
	return -1;
    }


    # Get queue status and counts
    #  the jobs running and total possible per queue -- Pass 1
    @result = `$STATUS`;
    foreach $line (@result){
	chomp($line);
	if ( $BMODE ==0 ){
	    # The purpose of this was to count the job slots i.e.
	    # total and running and number of un-available nodes
	    @items = split("%",$line);

	    if( $items[1] ne "unavailable"){
		$i = $items[4];
		$CRSQ::TOT{$i}       += $items[2];
		$CRSQ::RUN{$i}       += $items[3];
		$NODES{$items[0]}    += $i;

		if ($i == $qnum && $CRSQ::fcdrop){  $CRSQ::TOT{$i}--;}
		if ($i != $qnum && $CRSQ::fldrop){  $CRSQ::TOT{$i}--;}

	    } else {
		# Number of un-available nodes for any reasons
		$NOTA++;
	    }
	} elsif ( $BMODE == 1 ){
	    # This will not give us the total number of un-available
	    # nodes and blabla ... so, we have to concentrate on the
	    # quantities we really need
	    @items = split(" ",$line);
	    next if ($line =~ m/queue,/);
	    #print "DEBUG [$line]\n";
	    $i = $items[0];
	    $CRSQ::TOT{$i}           += $items[1];
	    $CRSQ::RUN{$i}           += $items[2];

	    # in this mode, it does not make sens to reduce count
	    # if the condition is not passed.
	    if ( $CRSQ::TOT{$i} > $CRSQ::RUN{$i} ){
		if ($i == $qnum && $CRSQ::fcdrop){  $CRSQ::TOT{$i}-- ;}
		if ($i != $qnum && $CRSQ::fldrop){  $CRSQ::TOT{$i}-- ;}
	    }
	}
    }


    # Calculate the total number of available jobs
    foreach $i (keys %CRSQ::TOT){
	if( $i == 0 && $BMODE == 0){ next;}
	#printf
	#    "Queue=%d Tot=%3d Run=%3d Dif=%3d\n",
	#    $i,$CRSQ::TOT{$i},$CRSQ::RUN{$i},
	#    ($CRSQ::TOT{$i}-$CRSQ::RUN{$i});

	$CRSQ::DIF{$i} = $CRSQ::TOT{$i}-$CRSQ::RUN{$i};
	$TOT += $CRSQ::TOT{$i};
	#print "DEBUG q=$i $CRSQ::TOT{$i} $CRSQ::RUN{$i} --> $CRSQ::DIF{$i} ($TOT)\n";
    }


    # Extra sanity check. If the first command did not return
    # any information, no need to continue ...
    if($TOT == 0){
	print "CRSQ :: Error on ".localtime().
	    ", none of the queues are available unavailable=$NOTA qcounts=".join(";",keys %CRSQ::TOT)."\n";
	return -1;
    }


    if ( $BMODE == 0){
	# This small test implies that the job files will be moved
	# after beeing off the queue.
	# There are cases (July 30th for example) when the CRS
	# queues goes beserk and the crs_status does not return
	# any information. This leads to a bad count of jobs and
	# available slots (0 actually) and subsequently, too many
	# submission ... The pattern find would prevent this.
	if( defined($pat) && ! defined($nchk) ){
	    @all = &Glob($pat);
	    if($#all > $CRSQ::PFACT*$TOT ){
		print
		    "CRSQ :: Warning on ".localtime().", there are ",
		    "$#all job files found as $pat\n";
		# Stop, this condition is ab-normal
		return -1;
	    }
	}
    }
    # No idea of what this would be in BMODE 1, need more experience


    # --- Pass 2
    # Get the number of jobs actually on their way
    # to those queues. We assume that we will extract a count
    # per node as well.

    @result = `$JOBINF`;

    foreach $line (@result){
	chomp($line);

	if ( $BMODE == 0 ){
	    # Mode
	    # 'staging' or 'staged' jobs would not
	    # necessarily show up in the -m list.
	    # -c shows all jobs. Initial logic was a major bug because
	    # it did not account for this CRS queue peculiarity.
	    # This part is relevant in no $drop mode only.
	    @items = split("%",$line);

	    # complete the list. Easier for later testing.
	    if( ! defined($NODES{$items[3]}) ){ $NODES{$items[3]} = 0;}

	    $i = $NODES{$items[3]};
	    if( $i != 0){
		# This node is knows, hold queue $i and is busy
		$CRSQ::FND{$i}++;
	    } else {
		# the best we can do is to count on the queue info
		$CRSQ::FND{$items[4]}++;
	    }

	} elsif ( $BMODE == 1 ) {
	    # In this mode, we have to trim each lines and split and
	    # in addition, we have no ways to know which jobs goes
	    # into which queue ...
	    $line =~ s/\s+/ /g;

	    next if ($line =~ m/^\#/);
	    next if ($line =~ m/\s+DONE/);

	    #  The first 2 are straight queue and number in (total)
	    @items = split(" ",$line);

	    #print "DEBUG :: Found q=$items[0] n=$items[1]\n";
	    $CRSQ::FND{$items[0]} = $items[1];
	}
    }


    # Recalculate the total number of real slots we may use
    $TOT = 0;
    foreach $i (keys %CRSQ::TOT){
	if( $i == 0){ next;}

	# sounds complicated ? it just say if to do something if
	# arguments are valid.
	if( ($i == $qnum && ! $drop)                         ||
	    ($i <= $qnum && $i >= ($qnum - $drop) && $drop)) {

	    if($DEBUG){
		printf
		    ">> Queue=%d Tot=%3d Run=%3d Found=%3d Diff=%3d\n",
		    $i,$CRSQ::TOT{$i},$CRSQ::RUN{$i},$CRSQ::FND{$i},$CRSQ::DIF{$i};
	    }

	    # check if something else has submitted jobs there
	    if ( $CRSQ::DIF{$i} < 0 ){
		if( $CRSQ::WARN{$i} ){
		    $CRSQ::DIF{$i} = - $CRSQ::DIF{$i};
		    print
			"CRSQ :: There are $CRSQ::DIF{$i} more jobs in queue $i ",
			"than what we intended to use on ".localtime()."\n";
		    $CRSQ::WARN{$i} = 0;
		}
		$CRSQ::DIF{$i}  = 0;

	    } elsif( $CRSQ::FND{$i}  > $CRSQ::RUN{$i}){
		# After the counting, we have found FND jobs but
		# RUN ones in pass 1 . Those are the un-accounted for
		# jobs in staged/staging or other weird states.

		# Make a diff adjustment
		if ( $CRSQ::DIF{$i} != 0 ){
		    $ratio = ($CRSQ::FND{$i}-$CRSQ::RUN{$i})/$CRSQ::DIF{$i}*100 ;
		} else {
		    $ratio = 100;
		}

		# The OFFSYNC value will decide on whether or not we
		# continue to submit jobs in this case.
		if( $ratio < $CRSQ::OFFSYNC ){
		    if( $CRSQ::WARN{$i} ){
			print
			    "CRSQ :: Queue $i not in sync but within margin ",
			    "(Found $CRSQ::FND{$i} vs $CRSQ::RUN{$i} running over ",
			    " $CRSQ::DIF{$i} slots). Adjusting on ".localtime()."\n";
			$CRSQ::WARN{$i} = 0;
		    }
		    $CRSQ::DIF{$i} -= ($CRSQ::FND{$i}-$CRSQ::RUN{$i});
		    if( $CRSQ::DIF{$i} < 0){ $CRSQ::DIF{$i} = 0;}
		} else {
		    if( $CRSQ::WARN{$i} ){
			print
			    "CRSQ :: Queue $i not in sync. Bootstrap ",
			    "found $CRSQ::FND{$i} but $CRSQ::RUN{$i} running ",
			    "on ".localtime()."\n";
			$CRSQ::WARN{$i} = 0;
		    }
		    $CRSQ::DIF{$i}  = 0;
		}

	    } else {
		# All OK (there are available slots) or nothing to do.
		$CRSQ::WARN{$i} = 1;

	    }


	    $TOT += $CRSQ::DIF{$i};

	} else {
	    # This needs to be reset for later use (submit takes
	    # advantage of this).
	    $CRSQ::DIF{$i} = 0;
	}
    }

    # mem cleanup
    undef(@items);
    undef(@result);

    if( $TOT != 0){
	print "CRSQ :: ";
	foreach $i (keys %CRSQ::TOT){
	    print "Queue=$i ($CRSQ::DIF{$i}) " if ($CRSQ::TOT{$i} != 0);
	}
	print "\n";
    }

    #die "$TOT\n";

    # We don't care of what is happening to the spill-over
    # queue pool. If the count is incorrect there, just set
    # to 0.
    return $TOT;
}



#
# Submit $jfile with priority $prio. Eventually use a queue
# shift of $drop.
#
# Note that the last 2 arguments are not mandatory if
# the getcnt() routine was called. If you chose to submit
# that way, no jobs will be submitted after the getcnt()
# stack gets exhausted. If you still want to persist in
# submitting something, you must at least specify a default
# $qnum value. This latest mode is not encouraged ...
#
# If $prio is passed negative, this routine will make a
# reverse-adjustement of priorities i.e. lower queue will
# have higher priorities (lower number).
#
#
sub CRSQ_submit
{
    my($jfile,$prio,$qnum,$drop)=@_;
    my($res,$i,$q,$cprio);

    # Our system is priority 100 based but BMODE 1 has 20
    # level of priorities. Priorities do not matter however
    # (condor alpha sort)
    if ($BMODE == 1){
	$prio = int($prio/5);
    }

    if ( ! -e $jfile){
	print
	    "CRSQ :: File $jfile does not exists. ",
	    "Cannot submit to queue $qnum\n";
	0;
    }
    if( ! defined($qnum) ){  $qnum = 0;}
    if( ! defined($drop) ){  $drop = 0;}
    $drop = abs($drop);

    if( $CRSQ::GETCNT ){
	#print "GETCNT() was called\n";
	foreach $i (sort {$b <=> $a} keys %CRSQ::DIF){
	    $q = $i;
	    if( $CRSQ::DIF{$i} != 0){
		$CRSQ::DIF{$i}--;
		last;
	    }
	}
	if( $q == 0 ){
	    # exhausted. Cannot make it that way
	    $CRSQ::GETCNT = 0;
	    # We will therefore comply with the exact command
	    # i.e. submitting in queue $qnum with $drop as
	    # arguments says. However, if we used that function
	    # with qnum=0, we have to leave now.
	    if( $qnum == 0){ return 0;}
	} else {
	    $drop = 1 if ($drop != 0);
	    #$drop = 0;
	    $qnum = $q;
	    #print "We have selected queue $q $CRSQ::DIF{$q}\n";
	}
    }

    # Priority adjustment or not
    if ($prio < 0){
	# Auto-adjust priority
	$cprio = (-$prio) + $q - 1;
    } else {
	$cprio = $prio;
    }

    if ( $BMODE == 0 ){
	$res = `$SUBMIT $jfile $cprio $qnum $drop`;
	$res =~ s/\n//g;
	if( $res =~ m/queue $qnum with priority $cprio/){
	    $qnum;
	} else {
	    if ( $res =~ m/(queue\s+)(\d+)/ ){
		print "CRSQ :: Failed to submit $jfile $qnum -> $2 => [$res]\n";
	    } else {
		print "CRSQ :: Failed to submit $jfile $qnum => [$res]\n";
	    }
	    0;
	}

    } elsif ( $BMODE == 1 ){
	# unfortuntaley, -drop in this system is not
	# controllable
	$res = `$QUEUEJ $jfile -q$qnum -p$cprio -drop`;
	chomp($res);
	if ($res eq ""){
	    print "CRSQ :: Something went wrong [$SUBMIT $jfile -q$qnum -p$cprio -drop]\n";
	    0;
	} else {
	    # the return value contains the real job
	    $rres = `$SUBMIT $res`;
	    chomp($rres);
	    if ($rres !~ m/the job is in status SUBMITTED/  &&
		$rres !~ m/submitted to cluster \d+/            ){
		print "CRSQ :: Do not know status [$rres] for [$SUBMIT $res]\n";
	    }
	}

	$qnum;

    } elsif ( $BMODE == 2 ){

	$res = `$QUEUEJ $jfile`;
      $rres = `$SUBMIT -s CREATED`;

    }
}



#
# Alter internal parameters
#

# Offset accepts a new value for $CRSQ::OFFSYNC (a %tage ofsset in the queue
# to be considered) and return the previous value (so one could use a re-entrent
# trick to restore values to default).
#
# If undef is returned, the input was not accepted.
#
sub CRSQ_Offset
{
    my($new)=@_;
    my($old);

    if ( defined($new) ){
	if ($new != 0){
	    $CRSQ::OFFSYNC = $new;
	    return $old;
	}
    }
    return undef;
}

# normalize and decode the queue number
sub CRSQ_Norm
{
    my($drop,$mode)=@_;

    #print "CRSQ :: Received $drop\n";
    if ( $drop >= 0 ){
	# Use flags to tell the code to do that and treat
	# all options.
	if ( $drop > 100){
	    # > 100, all queues at MAX
	    if ($mode == 1){
		$CRSQ::fcdrop = 0;
		$CRSQ::fldrop = 0;
	    }
	    $drop -= 100;
	} else {
	    # normal case, current queue is MAX, drop by
	    # one for lower queues.
	    if ($mode == 1){
		$CRSQ::fcdrop = 0;
		$CRSQ::fldrop = 1;
	    }
	}
    } else {
	# Case negative
	if ( $drop < -100){
	    # For the upper queue, use only MAX-1 and
	    # MAX for the lower ones.
	    if ($mode == 1){
		$CRSQ::fcdrop = 1;
		$CRSQ::fldrop = 0;
	    }
	    $drop += 100;
	} else {
	    # Drop by 1 for all queues
	    if ($mode == 1){
		$CRSQ::fcdrop = 1;
		$CRSQ::fldrop = 1;
	    }
	}
    }
    $drop = abs($drop);

    return $drop;
}


#
# Stupid blabla routine
#
sub Exceed
{
    my($param,$max)=@_;

    print "CRSQ :: Received a parameter exceeding expected range. $param < $max\n";
    return 0;
}

sub Glob
{
    my($pat)=@_;
    my(@all);

    # support perl patterns
    if( $pat =~ /\.\*/){
	# a .* like .
	if ( opendir(DIR,".") ){
	    @all = grep { /$pat/ } readdir(DIR);
	    close(DIR);
	}
    } else {
	# use glob although this may fail
	@all = glob($pat);
    }
    @all;
}


1;

