#!/usr/local/bin/perl

use strict;
use warnings;

my $SITER   = 0;
my $NITER   = 8;
my $STANDBY = 1800;
my $OUTNAME = "fmsIter";
my $RESULTS = "Iterations";
my $RUNLIST = $ARGV[0];

#Check requirements before execution
unless (-e './FmsCellStat.txt')    { die; }
unless (-e './FmsGainCorr.txt')    { die; }
unless (-e './FmsMapBase.txt')     { die; }
unless (-e './FmsMapBitShift.txt') { die; }
unless (-e './calibFms.C')         { die; }
unless (-e './calibFmsShow.C')     { die; }
unless (-e './calibFmsTools.C')    { die; }

for (my $a=$SITER; $a<=$NITER; $a++)
{
	if ($a==0) { system ("mkdir $RESULTS"); }

	print "===============================================================\n";
	print "Starting iteration... $a/$NITER\n";

	#Scheduler
	#--------------------------------------------

	system ("./SchedulerSubmit.csh ${OUTNAME}_$a $RUNLIST");
	sleep $STANDBY;

	my $NBNeck = 0;
	while (1)
	{
		if ($NBNeck > 10)
		{
			print "Kill Condor jobs causing bottleneck... \n";
			system ("condor_rm $ENV{'USER'}");
			last;
		}

        my $Status = `condor_q $ENV{'USER'} | grep running | head -1`;
		if ($Status ne "")
		{
            my $JobsRun  = `printf "$Status" | sed 's/.*idle\, //' | cut -d ' ' -f 1`;
			my $JobsIdle = `printf "$Status" | sed 's/.*removed\, //' | cut -d ' ' -f 1`;
			my $JobsAll  = $JobsRun + $JobsIdle;

			print "# of Scheduler jobs running/idle: $JobsAll\n";
			if ($JobsAll <= 500) { $STANDBY = 600; }
			if ($JobsAll <= 100) { $STANDBY = 300; }
			if ($JobsAll <=  10) { $NBNeck++; }
			if ($JobsAll ==   0) { last; }
		}
		else { print "Cannot retrieve Condor status!\n"; }

		sleep $STANDBY;
	}#Monitor Scheduler running

	#Merge
	#--------------------------------------------

	print "Scheduler job for iteration $a finished. Start 1st merging process...\n";
	system ("./MergeByCondor.csh ./out_${OUTNAME}_$a $RUNLIST");
	sleep $STANDBY;

	while (1)
	{
        my $Status = `condor_q $ENV{'USER'} | grep running | head -1`;
		if ($Status ne "")
		{
            my $JobsRun  = `printf "$Status" | sed 's/.*idle\, //' | cut -d ' ' -f 1`;
			my $JobsIdle = `printf "$Status" | sed 's/.*removed\, //' | cut -d ' ' -f 1`;
			my $JobsAll  = $JobsRun + $JobsIdle;
			if ($JobsAll == 0) { last; }
		}
		else { print "Cannot retrieve Condor status!\n"; }

		sleep $STANDBY;
	}#Monitor 1st merge

	print "Start 2nd merging process...\n";
	system ("./Merge.csh merged");
	system ("mv merged_merged.root fmsCalib_$a.root");

	#Fit on pi0 mass, then update Gain correction
	#--------------------------------------------

	system ("root -l -b -q calibFms.C+'($a)'");
	system ("mv fmsCalib_$a.root out_fmsCalib_$a.root $RESULTS");
	system ("rm -r out_${OUTNAME}_$a merge* log_* sched* *.session.xml report");
}#a, iteration

system ("mv FmsCellStat.txt $RESULTS/FmsCellStat_fin.txt");
system ("mv FmsGainCorr.txt $RESULTS/FmsGainCorr_fin.txt");
