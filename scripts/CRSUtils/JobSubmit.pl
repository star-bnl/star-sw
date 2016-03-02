#!/usr/bin/env perl
#
# This script will fetch records fron the o-database
# and create a job file and submit it.
# This script is meant to run under starreco and on
# a rcrs nodes from which we can actually submit a job
# and finally, only after a cd to the working directory
# where the scripts will be submitted.
#
# The number of jobs we can submit is automatically
# determined byt the number of available queues.
#
# A cronjob should call this script regularly using
# arguments
#  % ScriptName [LibVersion] [NumEvt] [{|^|+|C|Z|X}targetDataDisk{:user}]
#     [CommaSeparatedChainOptions]  [Queue] [SpillMode]
# or
#  % ScriptName [LibVersion] [NumEvt] [Option]
#
# If targetDataDisk starts with a ^, the jobs are only
# taken from the top of the DDB pile (latest entries),
# otherwise, the jobs will be taken from top to bottom
# which will have the net effect to go through all jobs
# if time allows.
#
# If + is specified, the run listed in a file named after
# $CONFF are first marked as new and submitted one after
# another. This mode slides in some extraneous runs in
# high priority mode (spill mode) and provide a convenient
# to push some runs through the FastOffline chain.
#
# The second syntax as a few options
#   Option=1      Move the log into the archive directory
#                 if the job is done.
#
# Default are currently
#    dev, 20, /star/data19/reco
#
# Note that the presence of a file named FastOff.quit will
# make this script to quit without any processing. This
# script itself handles clashes between multiple execution
# via  afile named FastOff.lock . This latest file, if created
# by hand, it may be automatically deleted if older than
# some arbitrary time. TO stop FastOffline, you should then
# use the FastOff.quit file mechanism instead.
#
# History
#  Fri Jul 27. dammit !! Did not take into account that
#              'staged' and 'staging' jobs are not showed
#              in -m mode. Got a situation with zillions
#              of submit. Now corrected.
#
# Sunday Sept 10. Added +/path syntax.
#
# Mon Dec 17 2001, chained to DCHAIN functioning on based on
#              a mandatory collision
#              type for submiting with the default chain. If
#              a chain is used as argument, this does not
#              apply.
# Jan 22 2002  Added mode 3 which performs calibration sweep
#              on ftype. We MUST use the proper ftype number
#              in the routine call i.e. a-priori known
#              what the laser files will be in our case ...
# Feb  9 2002  Modified for support of several LIB dependent
#              job files simply using JobSubmit$LIB.lis .
#              Allows for many cron-tab entries and several
#              jobs in //.
#
# Jan 2005     Added HPSS output mode
# May 2005     Added disk range syntax support (delegation to bfcca)
# Apr 2006     $ltarget/FreeSpace mechanism for PANASAS, changed
#              the meaning of express.
# Sep 2008     Added JS_DEBUG for additional messages
#
#

use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use RunDAQ;
use CRSQueues;


$ThisYear = 2016;                 # Block to consider. Completely artificial
                                  # and used to preserve older options in if
                                  # block along with current option.
$HPSS     = 1;                    # turn to 0 for UNIX staging only
                                  # HPSS staging was requested for Run5

$DSKLOG = "/star/rcf";            # disk where the logs go
# $DSKLOG = "/star/data36";

# disable at first
# $ThisYear = -1;
$TM = localtime();
$TM =~ s/[ :]/_/g;

# Self-sorted vars
$SELF  =  $0;
$SELF  =~ s/.*\///;
$SELF  =~ s/\..*//;
$SSELF = $SELF;
$SELF .= " :: ".$TM;

# default values global (unless overwritten)
@EXPRESS = undef;  $EXPRESS_W  =  70;
$ZEROBIAS= 0;      $ZEROBIAS_W =  30;

# this does not have a weight
$PHYSTP2 = 0;

# If defined, command line argument will be ignored and this
# will be the ultimate maximum number of events processed.
# DO NOT SET IT HERE but in the diverse blocks.
$MAXEVT  = 0;


# Second layer throttling would do one file per run within the
# regular mode.
$THROTTLE = 2;

# Do not change this value. Set by year, this is the default.
$TREEMODE = 0;

#### SET THIS TO 1 TO DEBUG NEW SCRIPTS / FEATURES - Can be done by year
$DEBUG    = 0;


# variables default value before they were implemented
$FRACTT = 100;                    # %tage sent when AllFiles is TRUE - feature starts 2009
$SKIPMC = "abcde";                # a fake MC tag - if stream match any, skip MC execlusion of event.root

#@QUARANTEEN=("data11")

#
# Default values Year9 data
# Removed 2002->2004 on 2009/02 J.Lauret (see cvs revisions for history)
#

my $MAXSLOTS = 200;
my $NSLOT = 0;
my $njobs = 0;
$TOT = 0;

my @STATES = ("RUNNING","QUEUED","SUBMITTED","STAGING","CREATED");


if ( $ThisYear == 2005 ){
    $TREEMODE= 1;
    $LIB     = "dev";

    $NUMEVT  = 100;
    #$MAXEVT  = 250;

    $TARGET  = "/star/data+08-09/reco";   # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those were made automatically guessed in 2005.
    # Previous years hardcoded values could remain as-is (will not change
    # as tables are already filled)
#    $LASERTP =  rdaq_string2ftype("laser");
    $PHYSTP  =  rdaq_string2ftype("physics");
    $PHYSTP2 =  rdaq_string2ftype("physics_adc"); # just comment them if you want them disabled
#    @EXPRESS = (rdaq_string2ftype("express"));
#    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    @USEQ    = (5,5,4);
    @SPILL   = (1,5,2);

    # Default chain -- P2005 does not include Corr4 but Corr3
    $DCHAIN{"AuAu"}           = "P2005,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,CMuDst,OShortR";
    $DCHAIN{"PPPP"}           = "P2005,ppOpt,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,CMuDst,OShortR";
    $DCHAIN{"CuCu"}           = "P2005,SCEbyE,OGridLeak,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,CMuDst,OShortR,OSpaceZ2";

    $DCALIB{"CuCu"}           = "OneSpaceCharge";

    # Default stand-alone auto-calib (works ONLY on $LASERTP files)
    $SCALIB{"AuAu"}           = "OptLaser";
    $SCALIB{"PPPP"}           = "OptLaser";
    $SCALIB{"CuCu"}           = "OptLaser";

    # ezTree production requires some conditions. We set them here.
    # ezTree uses the Xtended Status index 1. See table in RunDAQ.pm
    $ID                       = 1;
    $XCONDITION{"Status"}     = 0;
    $XCONDITION{"XStatus$ID"} = 0;

    if ( ($tmp = rdaq_string2trgs("ppProductionMinBias")) != 0){
    	# Self adapting
    	$XCONDITION{"TrgSetup"} = $tmp;
    }

} elsif ( $ThisYear == 2006 ) {
    # ... blabla like year5 ...
    $TREEMODE= 1;
    $LIB     = "dev";

    $NUMEVT  = 100;
    $MINEVT  = 200;

    $TARGET  = "/star/data09/reco";       # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those were made automatically guessed in 2005.
    # Previous years hardcoded values could remain as-is (will not change
    # as tables are already filled)
    $LASERTP =  rdaq_string2ftype("laser");

    $PHYSTP  =  rdaq_string2ftype("physics");
    $PHYSTP2 =  rdaq_string2ftype("physics_adc"); # just comment them if you want them disabled
    @EXPRESS = (
		rdaq_string2ftype("express"),
		rdaq_string2ftype("jpsi"),
		rdaq_string2ftype("upsilon"),
		rdaq_string2ftype("btag"),
		rdaq_string2ftype("muon")
		);
    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    @USEQ    = (5,5,5);
    @SPILL   = (0,2,4);

    # Default chain -- P2005 does not include Corr4 but Corr3
    #$DCHAIN{"PPPP"}           = "pp2006a,ittf,ezTree";
    $DCHAIN{"PPPP"}           = "pp2006b,ittf,ezTree";

    # Default stand-alone auto-calib (works ONLY on $LASERTP files)
    $SCALIB{"PPPP"}           = "OptLaser";

    # ezTree production requires some conditions. We set them here.
    # ezTree uses the Xtended Status index 1. See table in RunDAQ.pm
    $ID                       = 1;
    $XCONDITION{"Status"}     = 0;
    $XCONDITION{"XStatus$ID"} = 0;

    # ...
    #if ( ($tmp = rdaq_string2trgs("minbiasSetup")) != 0){
    if ( ($tmp = rdaq_string2trgs("pp2006MinBias")) != 0){
    	# Self adapting
    	$XCONDITION{"TrgSetup"} = $tmp;
    }

} elsif ( $ThisYear == 2007 ) {
    # ... blabla like year6 ...
    $TREEMODE= 1;
    $LIB     = "dev";

    $NUMEVT  = 100;
    $MINEVT  = 200;

    $TARGET  = "/star/data09/reco";       # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those were made automatically guessed in 2005.
    # Previous years hardcoded values could remain as-is (will not change
    # as tables are already filled)
    $LASERTP =  rdaq_string2ftype("laser");

    $PHYSTP  =  rdaq_string2ftype("physics");
    $PHYSTP2 =
	rdaq_string2ftype("physics_adc")."|".
	rdaq_string2ftype("upsilon")."|".
	rdaq_string2ftype("btag");

    @EXPRESS = (
		rdaq_string2ftype("express"),
		rdaq_string2ftype("jpsi"),
		rdaq_string2ftype("gamma"),
		rdaq_string2ftype("mtd"),
		rdaq_string2ftype("muon"),
		rdaq_string2ftype("upcjpsi")
		);
    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    # Order is: regular, bypass, calib
    @USEQ    = (5,5,5);
    @SPILL   = (0,4,4);

    # Default chain
    # $DCHAIN{"AuAu"}           = "p2007a,alltrigger,ittf,ezTree";
    # Changed Thu Apr 26 13:51:28 EDT 2007
#    $DCHAIN{"AuAu"}           = "p2007b,alltrigger,ittf,ezTree";
    $DCHAIN{"AuAu"}           = "p2007b,ittf,pmdRaw,ezTree";

    # Default stand-alone auto-calib (works ONLY on $LASERTP files)
    $SCALIB{"AuAu"}           = "OptLaser";

    # ezTree production requires some conditions. We set them here.
    # ezTree uses the Xtended Status index 1. See table in RunDAQ.pm
    $ID                       = 1;
    $XCONDITION{"Status"}     = 0;
    $XCONDITION{"XStatus$ID"} = 0;

    # ...
    # EzTree were processed for this trigger before
    #
    # if ( ($tmp = rdaq_string2trgs("pp2006MinBias")) != 0){
    #	# Self adapting
    #	$XCONDITION{"TrgSetup"} = $tmp;
    # }

} elsif ( $ThisYear == 2008 ) {
    $TREEMODE= 1;
    $LIB     = "dev";

    $NUMEVT  = 100;
    $MINEVT  = 200;

    $TARGET  = "/star/data09/reco";       # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those are taken from previous yera - agreed upon as per rate, etc...
    # and documented on our Web pages.
    $LASERTP =
	rdaq_string2ftype("laser")."|".
	rdaq_string2ftype("laser_adc");

    $PHYSTP  =  rdaq_string2ftype("physics");
    $PHYSTP2 =
	rdaq_string2ftype("physics_adc")."|".
	rdaq_string2ftype("upsilon")."|".
	rdaq_string2ftype("btag");

    @EXPRESS = (
		rdaq_string2ftype("express"),
		rdaq_string2ftype("jpsi"),
		rdaq_string2ftype("gamma"),
		rdaq_string2ftype("mtd"),
		rdaq_string2ftype("muon"),
		rdaq_string2ftype("upcjpsi")
		);
    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    # Added for testing purposes
    $ZEROBIAS_W = 0;
    $EXPRESS_W  = 0;


    # Order is: regular, bypass, calib
    @USEQ    = (5,  5,5);
    @SPILL   = (0,105,4);


    # Chain for 2008 starts here
    $DCHAIN{"dAu"}  = "P2008b,ITTF,BEmcChkStat";
    $SCALIB{"dAu"}  = "OptLaser";

    # at least, p+p calib
    $DCHAIN{"PPPP"} = "pp2008a,ITTF,BEmcChkStat,QAalltrigs";
#    $DCHAIN{"PPPP"} = "pp2008a,ITTF,BEmcChkStat";
    $SCALIB{"PPPP"} = "OptLaser";
    $DCHAIN{"AuAu"} = "pp2008a,ITTF,BEmcChkStat,QAalltrigs";
    $SCALIB{"AuAu"} = "OptLaser";


} elsif ( $ThisYear == 2009 ) {
    $TREEMODE= 1;
    $LIB     = "dev";

    $NUMEVT  = 100;
    $MINEVT  = 200;
    $FRACTT  =  33;

    $TARGET  = "/star/data09/reco";       # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those are taken from previous yera - agreed upon as per rate, etc...
    # and documented on our Web pages.
    $LASERTP =
	rdaq_string2ftype("laser")."|".
	rdaq_string2ftype("laser_adc");

    $PHYSTP  =  rdaq_string2ftype("physics");
    $PHYSTP2 =
        rdaq_string2ftype("physics_adc")."|".
        rdaq_string2ftype("upsilon").    "|".
        rdaq_string2ftype("minbias").    "|".
        rdaq_string2ftype("minbias_adc")."|".
        rdaq_string2ftype("btag");

    @EXPRESS = (
		rdaq_string2ftype("express"),
		rdaq_string2ftype("jpsi"),
		rdaq_string2ftype("gamma"),
		rdaq_string2ftype("mtd"),
		rdaq_string2ftype("muon"),
		rdaq_string2ftype("upcjpsi")
		);
    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    # Added for testing purposes
    $ZEROBIAS_W = 0;
    $EXPRESS_W  = 0;


    # Order is: regular, bypass, calib
    @USEQ    = (5,  5,5);
    @SPILL   = (0,105,4);


    # at least, p+p calib
    $DCHAIN{"PPPP"} = "pp2009a,ITTF,BEmcChkStat,QAalltrigs,btofDat,Corr3,-hitfilt";
#    $DCHAIN{"PPPP"} = "pp2009a,ITTF,BEmcChkStat,btofDat,Corr3,-hitfilt";
   # $DCHAIN{"PPPP"} = "pp2009b,ITTF,BEmcChkStat,QAalltrigs,btofDat"; # <-- switch to this if trgd crash
    $SCALIB{"PPPP"} = "OptLaser";


} elsif ( $ThisYear == 2010 ) {
    $TREEMODE= 1;
    $LIB     = "dev";

    $NUMEVT  = 100;
    $MINEVT  = 200;
    $FRACTT  =  33;

    $TARGET  = "/star/data09/reco";       # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those are taken from previous yera - agreed upon as per rate, etc...
    # and documented on our Web pages.
    $LASERTP =
	rdaq_string2ftype("laser")."|".
	rdaq_string2ftype("laser_adc");

    $PHYSTP  =  rdaq_string2ftype("physics");
    $PHYSTP2 =
        rdaq_string2ftype("physics_adc")."|".
        rdaq_string2ftype("upsilon").    "|".
        rdaq_string2ftype("minbias").    "|".
	rdaq_string2ftype("upc").        "|".  # <--
        rdaq_string2ftype("minbias_adc");


    @EXPRESS = (
		rdaq_string2ftype("express"),
		rdaq_string2ftype("jpsi"),
		rdaq_string2ftype("btag"),
		rdaq_string2ftype("gamma"),
		rdaq_string2ftype("mtd"),
	        rdaq_string2ftype("hlt"),
		rdaq_string2ftype("muon"),
		rdaq_string2ftype("upcjpsi"),
		#rdaq_string2ftype("upc"),    # <---
                rdaq_string2ftype("ht"),
                rdaq_string2ftype("atomcules"),
                rdaq_string2ftype("pmdftp"),
                rdaq_string2ftype("monitor")
		);
    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    # Added for testing purposes
    $ZEROBIAS_W = 0;
    $EXPRESS_W  = 20;


    # Order is: regular, bypass, calib
    @USEQ    = (5,  5, 5);
    @SPILL   = (0,105, 4);


    # at least, p+p calib
#    $DCHAIN{"AuAu"} = "P2010a,btof,BEmcChkStat,QAalltrigs,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
    $DCHAIN{"AuAu"} = "P2010a,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
    $DCHAIN{"PPPP"} = "pp2010a,btof,VFPPVnoCTB,beamline,BEmcChkStat,QAalltrigs,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";

    # allow chain switch on condition matching
    # introduced in 2010, syntax is
    #    collision;name-match;... (could be other match later)
    #
    # Note that an empty string for a match will always pass the selection (by design)
    # and a partial match such as "Au" will also pass the criteria.
    #
    $MCHAIN{"AuAu;upc"}  = $DCHAIN{"PPPP"};

    $SCALIB{"AuAu"}      = "OptLaser";
    $SCALIB{"PPPP"}      = "OptLaser";


    # group years that were alike
} elsif ( $ThisYear == 2011 || 
	  $ThisYear == 2012 || 
	  $ThisYear == 2013 || 
	  $ThisYear == 2014 ||
          $ThisYear == 2015 ||
          $ThisYear == 2016 ) {
    $TREEMODE= 1;
    $LIB     = "dev";

    $MAXEVT  = 1000;
    $NUMEVT  = 1000;
    $MINEVT  = 200;
    $FRACTT  =  33;
    $SKIPMC  = "st_we";                   # keep lowercase

    $TARGET  = "/star/data09/reco";       # This is ONLY a default value.
                                          # Overwritten by ARGV (see crontab)

    # Those are taken from previous yera - agreed upon as per rate, etc...
    # and documented on our Web pages.
    $LASERTP =
	rdaq_string2ftype("laser")."|".
	rdaq_string2ftype("laser_adc");

    $PHYSTP  =  
	rdaq_string2ftype("physics")    ."|".
        rdaq_string2ftype("physics_adc");

    $PHYSTP2 =
        rdaq_string2ftype("pxl")        ."|".  # <-- appeared in 2013 run
        rdaq_string2ftype("pxl_adc")    ."|".  # <-- appeared in 2013 run
        rdaq_string2ftype("upsilon")    ."|".
        rdaq_string2ftype("minbias")    ."|".
        rdaq_string2ftype("himult")     ."|".
	rdaq_string2ftype("upc")        ."|".  # <-- moved here as data was large in 2011
        rdaq_string2ftype("minbias_adc")."|".
	rdaq_string2ftype("mtd")        ."|".  # <-- moved here in 2015
	rdaq_string2ftype("mtd_adc");


    if ( $ThisYear < 2016){
	@EXPRESS = (
	        rdaq_string2ftype("rp"),         # <-- new in 2015, 
		rdaq_string2ftype("express"),
		rdaq_string2ftype("jpsi"),
		rdaq_string2ftype("btag"),
		rdaq_string2ftype("WE"),         # <-- added 2013/05
		rdaq_string2ftype("centralpro"), # <-- was added in U+U, 2012
		rdaq_string2ftype("gamma"),
	        rdaq_string2ftype("hlt"),
		rdaq_string2ftype("muon"),
		rdaq_string2ftype("upc"),
		rdaq_string2ftype("upcjpsi"),
                rdaq_string2ftype("ht"),
                rdaq_string2ftype("atomcules"),
                rdaq_string2ftype("monitor")
		);
    } else {
	# Express became too numerous - send a note to software coordinators
	# that we would start WITHOUT express processing files and each should
	# be well justified. softcoord/267
	@EXPRESS = ();
    }

    $ZEROBIAS=  rdaq_string2ftype("zerobias");

    # Weights for submissions, %tage - 0 indicates at max 1
    $ZEROBIAS_W = 0;
    $EXPRESS_W  = 20;


    # format for new X mode is different for the array
    # must be an even number
    @PIPEOFF =  (
		 rdaq_string2ftype("W"),      100,
		 rdaq_string2ftype("W_adc"),  100,
		 rdaq_string2ftype("zerobias"),20,
		 rdaq_string2ftype("physics"),  5
		 );
    # $XCONDITION{"Status"}     = 0;
    $ID                       = 1;
    $XCONDITION{"XStatus$ID"} = 0;
    $XCONDITION{"NumEvt"}     = "> $MINEVT";



    # Order is: regular, bypass, calib - do not change
    @USEQ    = (5,   5,    5);
    @SPILL   = (0,   4,  105);


    # at least, p+p calib
    #$DCHAIN{"AuAu"} = "P2010a,btof,BEmcChkStat,QAalltrigs,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
    if ( $ThisYear == 2011 ){
	$DCHAIN{"AuAu"} = "P2011a,mtdDat,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
	$DCHAIN{"PPPP"} = "pp2011a,btof,VFPPVnoCTB,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
    } elsif ( $ThisYear == 2012 ) {
	# QAalltrigs
	$DCHAIN{"AuAu"} = "P2012a,AgML,mtdDat,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
	$DCHAIN{"PPPP"} = "pp2012a,AgML,mtdDat,btof,fmsDat,VFPPVnoCTB,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
	$DCHAIN{"UU"}   = $DCHAIN{"AuAu"};
	$DCHAIN{"CuAu"} = $DCHAIN{"AuAu"};
    } elsif ($ThisYear == 2013) {
	# 2013
	$DCHAIN{"PPPP"} = 
	    "pp2013a,mtd,btof,fmsDat,fgt,fgtPoint,VFPPVnoCTB,beamline,BEmcChkStat,".
	    "Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
    } elsif ($ThisYear == 2014) {

        $DCHAIN{"He3Au"} =
            "P2014a,mtd,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
        $DCHAIN{"AuAu"} = "P2014a,mtd,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";

   } elsif ($ThisYear == 2015) {

        $DCHAIN{"PPPP"} = "pp2015,btof,mtd,pp2pp,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt";
        $DCHAIN{"PPAu"} = "P2015,btof,mtd,pp2pp,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt";
        $DCHAIN{"PPAl"} = "P2015,btof,mtd,pp2pp,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt";
   } elsif ($ThisYear == 2016) {
        $DCHAIN{"AuAu"} = "P2016,btof,mtd,pxlHit,istHit,sstHit,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt";

   } else {
        $DCHAIN{"PPPP"} =
            "P2016,btof,mtd,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt";
        $DCHAIN{"AuAu"} = "P2016,btof,mtd,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt";
    }


    # allow chain switch on condition matching
    # introduced in 2010, syntax is
    #    collision;name-match;... (could be other match later)
    #
    # Note that an empty string for a match will always pass the selection (by design)
    # and a partial match such as "Au" will also pass the criteria.
    #
    $MCHAIN{"AuAu;upc"}  = $DCHAIN{"PPPP"};
    $MCHAIN{"UU;upc"}    = $DCHAIN{"PPPP"};
    $MCHAIN{"CuAu;upc"}  = $DCHAIN{"PPPP"};
    $MCHAIN{"pppp;we"}   = $DCHAIN{"PPPP"}.",KeepFgtHit,evout"; # <-- requested by the FGT folks, 2013/05

    $SCALIB{"AuAu"}      = "OptLaser";
    $SCALIB{"PPPP"}      = "OptLaser";
    $SCALIB{"UU"}        = "OptLaser";
    $SCALIB{"CuAu"}      = "OptLaser";

} else {
    # Well, at first you may get that message ... should tell you that
    # you have to add some default values.
    print "$SELF : Unknown Year $ThisYear\n";
    &Exit();
}




$CHAIN   = "";

$LIB     = shift(@ARGV) if ( @ARGV );
$NUMEVT  = shift(@ARGV) if ( @ARGV );
$TARGET  = shift(@ARGV) if ( @ARGV );
$CHAIN   = shift(@ARGV) if ( @ARGV );

$tmpUQ   = shift(@ARGV) if ( @ARGV );
$tmpSP   = shift(@ARGV) if ( @ARGV );


# DO NOT MODIFY THIS unless all STAR scripts have moved elsewhere
$SPATH    = "/afs/rhic.bnl.gov/star/packages/scripts";
$DBLBNAME = "dbLoadBalancerLocalConfig_nightly.xml";

# if we wait 1 minute between submit, and our cron
# tab executes this once every 20 minutes, max the
# return number of slots to $MAXCNT.
$SCRATCH = ".";
$LOCKF   = "FastOff.lock";
$QUITF   = "FastOff.quit";
$CONFF   = "JobSubmit$LIB.lis";
$PRIORITY= 50;                        # default queue priority    (old=100 [max], new 50 (lower the better))
$SLEEPT  =  1;                        # sleep time between submit (old=10)
$MAXCNT  = 50;                        # max job to send in a pass
$RATIO   =  2;                        # time drop down for mode + (2=twice faster)
$MAXFILL = 95;                        # max disk occupancy
$FUZZ4C  =  3;                        # for C mode, margin is higher MAXFILL+FUZZ4C <= 100 best ;-)
$MINEVT  =  0 if (!defined($MINEVT)); # minimum number of events to consider



# Check if the quit file is present
if ( -e $QUITF){
    print "$SELF : $QUITF detected I have been asked to skip processing\n";
    rdaq_set_message($SSELF,"$QUITF detected","I have been asked to skip processing");
    &Exit();
}

# be sure to turn it ON
# if (rdaq_toggle_debug()){ rdaq_toggle_debug();}
# if ($DEBUG){
#    rdaq_toggle_debug(1);
# }

# Global condition will exclude from accidental processing of junk
# puslers or lasers types. Note that EXPRESS are NOT added as they
# will be grabbed FIRST (then, if there is room, some other files).
$COND = "$PHYSTP";
if ($PHYSTP2 != 0){ $COND .= "|$PHYSTP2";}


#print "$SELF : Target is $TARGET\n";
#
# Check space on the target disk
#
if ($TARGET !~ m/^\d+$/){
    $target = $TARGET;
    $target =~ s/^\+//;
    $target =~ s/^C//;   # BEWARE !! hack here to check disk presence
    $target =~ s/^Z//;
    $target =~ s/^X//;
    $target =~ s/^\^//;
    $target =~ s/:.*//;

    # print "DEBUG $target ".substr($target,1,1)."\n ";

    if ( substr($target,1,1) eq "/" &&  substr($target,0,1) =~ m/\w/){
	print "$SELF : Method ".substr($target,0,1)." in target $TARGET not implemented\n";
	exit;
    }

    #
    # Support disk range and spanning. Syntax follows the same syntax
    # than bfcca i.e. /star/data+XX-YY where XX and YY are numbers and
    # the range will be defined as [XX,YY]
    #
    if ( $target =~ m/(.*)(\+)(\d+)(-)(\d+)(.*)/){
	$disk= $1;
	$low = $3;
	$high= $5;
	$last= $6;
    } else {
	$disk= $target;
	$low = $high = 0;
	$last= "";
    }

    $OK = 0==1;
    for( $i=$low ; $i <= $high ; $i++){
	if ( $i != 0){  $ltarget = sprintf("%s%2.2d%s",$disk,$i,$last);}
	else         {  $ltarget = $target;}

	if ( ! -e "$ltarget"){
	    print "$SELF : $ltarget does not exists\n";
	    rdaq_set_message($SSELF,
			     "Target disk space warning",
			     "$ltarget does not exists");
	    next;
	}

	#
	# FreeSpace file is generated from outside and especially from
	# FastOffCheck.pl which runs on the CAS farm nor on the rcrs node.
	# The main goal of this approach is that we had (pre-2007) PanFS
	# and the size could be reliably determined only from the farm
	# and not the rcrs node, hence an information passing mechanism
	# was designed.
	#
	$space = "";
	
	# input containing hints on disk occupancy and avoid a df
	if ( -e "$ltarget/FreeSpace"){
	    $delta = time()-(stat("$ltarget/FreeSpace"))[10];
	    if ( $delta < 3600){
		print "Reading free space from $ltarget/FreeSpace\n";
		open(FI,"$ltarget/FreeSpace");
		chomp($space=<FI>);
		close(FI);
	    }
	}
	if ( $space eq ""){
	    #print "Checking $ltarget\n";
	    chomp($space = `/bin/df -k $ltarget`);
	    $space =~ m/(.* )(\d+)(%.*)/;
	    $space =  $2;
	}

	#+
	# params to control disk
	#-
	if ( -e "$ltarget/Threshold"){
	    open(FI,"$ltarget/Threshold");
	    while ( defined($cline = <FI> ) ){
		chomp($cline);
		if ( $cline =~ m/(MAXFILL=)(\d+)/ ){
		    $MAXFILL = $2;
		    print "$SELF : Target=$TARGET Config sets MAXFILL to $MAXFILL\n"; 
		}
		# some other params
	    }
	    close(FI);
	}



	if ( $TARGET =~ m/^C/ ){
	    if ( $space >= $MAXFILL+$FUZZ4C){
		print "$SELF : C mode - disk $ltarget is $space % full\n";
		rdaq_set_message($SSELF,"Target disk space notice","C mode - $ltarget is $space % full");
	    } else {
		if ( $space >= $MAXFILL ){
		    # provide advance warning
		    rdaq_set_message($SSELF,
				     "Target disk space warning",
				     "$ltarget is $space % full, ".($MAXFILL-$space+$FUZZ4C)." % left before C mode stalls");
		}
		$OK = 1==1;
	    }

	} elsif ($space >= $MAXFILL ){
	    print "$SELF : Target disk $ltarget is $space % full\n";
	    rdaq_set_message($SSELF,"Target disk space notice","$ltarget is $space % full");

	} else {
	    # only one disk OK warrant a full OK
	    print "$SELF : Target disk $ltarget is $space < $MAXFILL (we shall proceed)\n";
	    $OK = 1==1;
	    last;
	}
    }

    if ( ! $OK){
	print "$SELF : Target disk(s) $target is/are full (baling out on ".localtime().")\n";
	rdaq_set_message($SSELF,"Disk Space problem","Target disk(s) $target is/are full (baling out)");
	&Exit();
    }
}



# Intermediate variable
$LIBV = $LIB;
if($LIB eq "cal"){ $LIB = "dev";}

$PAT = "$LIB"."_*_st_*";

# lock file mechanism prevents multiple execution of this
# script. However, prevent from crash and left over lock
# file. Notethat TARGET=1 also remove the LOCK file.
if ( -e $LOCKF){
    $date = time()-(stat($LOCKF))[9];
    if ($date > 3600){
	print "$SELF : removing $LOCKF (older than 3600 seconds)\n";
	rdaq_set_message($SSELF,"Recovery","Removing $LOCKF (older than 3600 seconds)");
	unlink($LOCKF);
    } else {
	print "$SELF : $LOCKF present. Skipping pass using target=$TARGET\n";
	rdaq_set_message($SSELF,"$LOCKF present. Skipping pass","using target=$TARGET");
    }
    &Exit();
}


# This will be a global selection
$SEL{"NumEvt"} = "> $MINEVT";



# Now go ...
if( $TARGET =~ m/^\// || $TARGET =~ m/^\^\// ){
    #
    # FAST OFFLINE regular mode
    #
    $TST = "regular mode";
    print "$SELF : FO $TST\n";

    undef(@Xfiles);
    undef(@Files);

    # Overwrite queue if necessary
    $USEQ[0] = $tmpUQ if ( defined($tmpUQ) );
    $SPILL[0]= $tmpSP if ( defined($tmpSP) );

    # Default mode is submit. Target is a path
    # get the number of possible jobs per queue.
#    $TOT = CRSQ_getcnt($USEQ[0],$SPILL[0],$PAT);
    $TOT = 1 if ($DEBUG || defined($ENV{FO_FORCE_SUBMIT}) );

#    print "$SELF : Mode=direct Queue count Tot=$TOT\n";

$njobs = 0;   
$NSLOT = 0;

    foreach my $jstate (@STATES) {
    $njobs = `/usr/bin/crs_job -long -s $jstate | grep Stdout | grep dev | wc -l` ;
    next if( !defined $njobs) ;
    $NSLOT +=  $njobs;
    }

    print "Number of jobs 1  ", $NSLOT, "\n";

#   $TOT = 100;
    $TOT = $MAXSLOTS - $NSLOT;
    if( $TOT < 0) {$TOT = 0};

    $time = localtime();
    if ( $TOT > 0 && ! -e $LOCKF ){
	open(FL,">$LOCKF");
	close(FL);
	# print "$SELF : We need to submit $TOT jobs\n";

	# Check $TARGET for sub-mode
	# If $TARGET starts with a ^, take only the top, otherwise
	# go into the 'crawling down the list' mode. We will start
	# with the second mode (gives an idea of how often we have
	# a dead time process earlier runs).

	if( ($obj = rdaq_open_odatabase()) ){
	    if( substr($TARGET,0,1) eq "^" ){
		# Simple with a perl module isn't it.
		print "$SELF : Top of the list only ...\n";
		$TARGET=~ s/\^//;
		if ($#EXPRESS != 0){
		    $num  = int($TOT*$EXPRESS_W/100)+1;
		    push(@Xfiles,rdaq_get_files($obj,-1,$num, 1,\%SEL,@EXPRESS));
		}
		if ($ZEROBIAS != 0){
		    $num = int($TOT*$ZEROBIAS_W/100)+1;
		    push(@Xfiles,rdaq_get_files($obj,-1,$num, 1,\%SEL,$ZEROBIAS));
		}
                # we may push into @Files up to 10 times more files than
		# nevessary. But please, check the logic further down i.e.
		# if we want to submit one file per run, we have to select
		# more files first and then sub-select from that pool. To do
		# that, we separate clearly Xfiles and files and loop over the
		# first array then the second ...
		# Be aware that if $THROTTLE is 0, there will be
		# no files of the regular type.
		$W = ($TOT-$#Xfiles+1);
		push(@Files,rdaq_get_files($obj,-1,$W*($THROTTLE?10:0), 1,\%SEL,$COND));

	    } else {
		my(@Fs);  # wil be used for randomizing the selection
		my($HOME)=$ENV{HOME};

		# ask only for status=0 files (will therefore
		# crawl-down the list).
		print "$SELF : Crawling down the list ...\n";

		if ($#EXPRESS != -1){
		    $num  = int($TOT*$EXPRESS_W/100)+1;
		    @Fs   = rdaq_get_files($obj,0,$num*($#EXPRESS+1), 1,\%SEL,@EXPRESS);
		    # open(FD,">>$HOME/debug.log"); print FD "\n";
		    # $z = 0; foreach my $f (@Fs){ @zozo = split(" ",$f); $z++; print FD "0 $z - $zozo[0]\n";} close(FD);
		    &RandArray(\@Fs);
		    # open(FD,">>$HOME/debug.log");
		    # $z = 0; foreach my $f (@Fs){ @zozo = split(" ",$f); $z++; print FD "1 $z - $zozo[0]\n";} close(FD);

		    push(@Xfiles,@Fs[0..$num]);
		}
		if ($ZEROBIAS != 0){
		    $num = int($TOT*$ZEROBIAS_W/100)+1;
		    push(@Xfiles,rdaq_get_files($obj,0,$num, 1,\%SEL,$ZEROBIAS));
		}
		# now the regular files
		$W  = ($TOT-$#Xfiles+1);
		# push(@Files,rdaq_get_files($obj,0,$W*($THROTTLE?10:0), 1,\%SEL,$COND));

		#
		# Randomize entries so we would pick the a representative sample
		# We cannot use the same trick than for streams because the st_physics
		# may dominate by a lot (making st_upc for example come at the end) so
		# we take equal amount of each and slice to $W instead.
		#
		my(@volatile) = split(/\|/,$COND);
		# count non-zero values
		$num = 0; foreach my $t (@volatile){  if ( $t ne ""){ $num++;}}
		#print "DEBUG --> We have $num in [$COND]\n";
		$num += 1 if ($num == 0);

		# now take individual trigger instead of a global mask
		undef(@Fs);
		foreach my $t (@volatile){
		    push(@Fs,rdaq_get_files($obj,0,$W, 1,\%SEL,$t));
		}

		#print "Got $#Fs files $HOME\n";
		#open(FD,">>$HOME/debug.log"); print FD "\n";
		#$z = 0;
		#foreach my $f (@Fs){ @zozo = split(" ",$f); $z++; print FD "0 $z - ".($z<$W?"Y":"N")." - $zozo[0]\n";}
		&RandArray(\@Fs);
		#$z = 0;
		#foreach my $f (@Fs){ @zozo = split(" ",$f); $z++; print FD "1 $z - ".($z<$W?"Y":"N")." - $zozo[0]\n";}
		#close(FD);
		push(@Files,@Fs[0..$W]);

	    }

	    print "$SELF : Xfiles=$#Xfiles Files=$#Files $TARGET\n";

	    if ( $DEBUG ){
		print "\tDEBUG Xfiles [".join(",",@Xfiles)."]\n" if ($#Xfiles != -1);
		print "\tDEBUG Files  [".join(",",@Files)."]\n"  if ($#Files  != -1);
	    }

	    if ( $#Xfiles == -1 && $#Files == -1){
		print "$SELF : We have no records to work with\n";
		&Exit();
	    }

	    undef(@files);
	    for($ii=0; $ii<=1 ; $ii++){
		# we loop twice and separate special cases and other cases
		@files = @Xfiles if ($ii==0);
		@files = @Files  if ($ii==1);

		if($#files != -1){
		    # scramble
		    #@files = &Scramble(@files);

		    print "$SELF : Checking ".($#files+1)." jobs\n";
		    undef(@OKFILES);             # will be filled by Submit
		    undef(@SKIPPED);

		    $kk    = $TOT;
		    $prun  = 0;
		    foreach $file (@files){
			# pattern match run-number / security pattern check
			# (should not really validate and a redundant test)
			if ( $file !~ m/(\D+)(\d+)(_raw)/){
			    print "$SELF : File [$file] did not match pattern at pass $ii ($#Xfiles,$#Files)\n";
			    push(@SKIPPED,$file);
			} else {
			    $run  = $2;
			    # print "DEBUG:: Deduced run=$run\n";

			    # Check run-number
			    if ($prun != $run){
				$count = 0;
				$prun  = $run;
			    } else {
				# print "DEBUG:: Same run but $count cmp $THROTTLE\n";
				$count++;
				if ( $count >= $THROTTLE && $THROTTLE != 0){ next;}
			    }
##### Submit
			    sleep($SLEEPT) if &Submit(0,$USEQ[0],$SPILL[0],
						      $file,$CHAIN,"Normal");
			    $MAXCNT--;
			    $kk--;
			}
			last if ($MAXCNT == 0);
			last if ($kk     == 0);
		    }
		    rdaq_set_files($obj,1,@OKFILES);
		    rdaq_set_chain($obj,$SCHAIN,@OKFILES);
		    rdaq_set_files($obj,4,@SKIPPED);
		} else {
		    # there is nothing to submit
		    print "$SELF : There is nothing to submit on $time for $TST\n";
		    rdaq_set_message($SSELF,"Submitted","There is nothing to submit at this time");
		}
	    }

	    rdaq_close_odatabase($obj);
	}
	if(-e $LOCKF){  unlink($LOCKF);}
    } else {
	rdaq_set_message($SSELF,"No slots available within range $USEQ[0] / $SPILL[0]","mode=direct");
    }



} elsif ($TARGET =~ m/(^\+)(.*)/ ) {
    #
    # FAST OFFLINE BYPASS
    #
    # Copied from mode 0. Can be merged ...
    #
    $TST = "bypass $TARGET";
    print "$SELF : FO $TST\n";

    $TARGET   = $2;
    print "$SELF : Target is now $TARGET - Queue $USEQ[1],$SPILL[1]\n";

    # Overwrite queue if necessary
    $USEQ[1] = $tmpUQ if ( defined($tmpUQ) );
    $SPILL[1]= $tmpSP if ( defined($tmpSP) );


    # In this mode, read a configuration file, select those runs
    # and submit them out.
    if( ! -e $CONFF){
	# no conf exit
	print "$SELF : mode=bypass ; could not find $CONFF at this moment\n";
	&Exit();
    }

    #print "$SELF : Opening DB & Checking lock\n";

    if( ! ($obj = rdaq_open_odatabase()) ){ &Exit("no db connection - abort");} # no ddb abort
    if( -e $LOCKF){ &Exit("$LOCKF exists - exiting");}                          # leave if lockf

    # read conf
    #print "$SELF : Reading $CONFF\n";
    open(FI,$CONFF) || die "Could not open $CONFF for read\n";
    chomp(@all = <FI>);
    close(FI);

    if ( $#all == -1){
	&Exit("Nothing to do $CONFF is empty?");
    } else {
	my($tmp)=($#all+1);
	rdaq_set_message($SSELF,"Found bypass requests","$tmp line".($tmp!=1?"s":"")." to consider ".join("::",@all));
    }

    # alter queue offset to allow for a higher fill
    print "$SELF :: Creating a queue Offset of 80 slots for this mode\n";
#    CRSQ_Offset(80);

    # get number of slots. Work is spill mode.
#    $TOT = 0;
#    if ($DEBUG){
#	$TOT++;
#    } else {
#	my ($DD)=CRSQ_Norm($SPILL[1],0);
#	print "$SELF : $USEQ[1]-$DD to $USEQ[1]\n";
#	for ($i=$USEQ[1]-$DD ; $i <= $USEQ[1] ; $i++){
#	    print "$SELF : Slow mode - Inspecting Queue=$i TotalSlots=$TOT\n";
#	    $Tot{$i} = CRSQ_getcnt($i,0,$PAT);
#	    $TOT += $Tot{$i};
#	}
#    }
#    # if the number of slots is non zero, push higher

#    $TOT   += 50;   # this about the number of nodes we have

$njobs = 0;   
$NSLOT = 0;
$TOT = 0;

    foreach my $jstate (@STATES) {
    $njobs = `/usr/bin/crs_job -long -s $jstate | grep Stdout | grep dev | wc -l` ;
    next if( !defined $njobs) ;
    $NSLOT +=  $njobs;
    }

    $TOT = $MAXSLOTS - $NSLOT;
    if( $TOT < 0) {$TOT = 0};

    print "Number of slots  3  ",$TOT,"   ", $MAXCNT, "\n";


     $TOT   += 50; 

#################################   ???? $MAXCNT

     $MAXCNT = 50 if ($MAXCNT < 50);
######################################

    # Ok or not ?
    if( $TOT > 0){
	# Lock file creation
	open(FL,">$LOCKF");
	close(FL);

	print "$SELF : We can slide in $TOT jobs\n" if ($#all != -1);
	foreach $line (@all){
	    # There are 2 possibilities. The run is new
	    # or has been marked for re-run. We will recognize
	    # a new run by a run number only on a line.
	    $line  =~ s/^\s*(.*?)\s*$/$1/;
	    @items = split(" ",$line);

	    # eliminate duplicate runs first
	    if ( defined($KRUN{$items[0]}) ){
		print "$SELF : Ignoring [$line] duplicate of previous record $KRUN{$items[0]} (already in?)\n";
		rdaq_set_message($SSELF,"Duplicate request","Ignoring [$line] - found previous record $KRUN{$items[0]}");
		next;
	    } else {
		$KRUN{$items[0]} = $line;
	    }

	    #+
	    # now, sort case 
	    #-
	    # The syntax
	    #    runumber #FSeq Chain Pat
	    # is the only one implied by the presence of a 4th element - grab the pattern
	    # or set it to any which ""
	    #
	    if (defined($items[3]) ){
		$patt = $items[3];
	    } else {
		$patt = "";
	    }

	    if($#items == 0 || $#items == 1){
		# Remains to be treated - this format is either
		#    runNumber 
		#    runNumber Chain
		#
		# In both cases, selection is -1 for the number of files
		# counter cnt is then the fullnumber of files in runNumber
		#
		print "$SELF : Run $line is new\n";

		if($#items == 1){
		    $cho = $items[1];
		    print "$SELF : Chain bypass $cho\n";
		} else {
		    if($CHAIN eq ""){ $CHAIN = "default";}
		    $cho = $CHAIN;
		}
		$SEL{"runNumber"} = $items[0];

		# foreach (keys %SEL){   print "XXXXDEBUG:: $_ $SEL{$_}\n";}


		@files = rdaq_get_orecords($obj,\%SEL,-1);

		if ($#files != -1){
		    print "$SELF : Resetting records to status=0 for $items[0]\n";
		    rdaq_set_files($obj,0,@files);      # Bypass: reset status to 0
		} else {
		    print "$SELF : There were no records of any kind for $items[0]\n";
		}

		# Get the count as being the total
		$run = $items[0];
		$cnt = $#files+1;

	    } else {
		# This format are the two remaining
		#    runumber #FSeq Chain 
		#    runumber #FSeq Chain Pat
		# Pat is already extracted ahead as it is the only format
		# with a 4th argument.
		# Additional syntax is 
		#  cnt == 0||-1 <=> get all files
		#  cnt < 0  <=> get all files and submit for all events
		#
		$run = $items[0];
		$cnt = $items[1];
		$cho = $items[2];
		if ( $cnt  == 0 || $cnt == -1){
		    $cnt = -1;
		} elsif ( $cnt < -1){
		    $cnt = -1;
		    $MAXEVT = $NUMEVT = -1;
		}
		
		$SEL{"runNumber"} = $run;
		$SEL{"Status"}    = 0;

		# if ($patt ne ""){  $cnt = $cnt*10;}
		@files = rdaq_get_orecords($obj,\%SEL,$cnt);
		# for all, due to the logic below, reset cnt to the number of
		# files returned
		$cnt = ($#files+1);
	    }

	    # OK. We have $cnt for that one and $TOT slots.
	    if ( $TOT != 0){
		print "$SELF : Working with $run -> $cnt and $TOT slots (Decrement MAX = $MAXCNT)\n";
		rdaq_set_message($SSELF,"Working with run","$run -> $cnt and $TOT slots");
	    }

	    # Submit no more than $TOT jobs
	    if($#files != -1){
		undef(@OKFILES);
		undef(@SKIPPED);
		$k = -1;
		foreach $file (@files){
		    last if ($MAXCNT <= 0);    # max jobs per pass

		    last if ($TOT <= 0);       # max available queues
		    last if (($cnt-$k) <= 0);  # max file seq for this run

		    #print "$SELF : Submitting [$file]\n";
		    $TOT = $TOT-1;
		    $k   = $k+1;

		    if ($patt ne ""){
			if ( $file !~ /$patt/ ){
			    print "$SELF : Skipping $file not matching $patt\n";
			    $TOT++;  # re-increment this since we skipped it
			    push(@SKIPPED,$file);
			    next;
			}
		    }

########   Submit
		    sleep($SLEEPT/$RATIO) if &Submit(1,$USEQ[1],$SPILL[1],
						     $file,$cho,"Bypass");

		    $MAXCNT--;
		}
		# Mark files as submitted
		rdaq_set_files($obj,1,@OKFILES);
		rdaq_set_chain($obj,$SCHAIN,@OKFILES);
		rdaq_set_files($obj,4,@SKIPPED);


		# Check if the $cnt matches $#OKFILES. This is
		# non-necessary check ; however, it allows us to
		# supress redundant runs from the $CONFF files when
		# the count is null. Can be done only when both
		# count matches.
		$tot  = $#OKFILES;
		$tot += $#SKIPPED if ($#SKIPPED != -1);
		if ($k == $tot ){ 
		    print "$SELF : count=$cnt candidates=$k skipped+OK=$tot\n";
		    # Save the remaining record count for that run
		    # When it will reach 0, no longer record that run.
		    if( $cnt > 0){
			$cnt = $cnt - $k if ($k != -1);
			push(@RECORDS,"$run $cnt $cho $patt");
		    }
		} else {
		    print "$SELF : Discrepency $k counted $tot success ".
			"($#OKFILES/$#SKIPPED)\n";
		    push(@RECORDS,"$run $cnt $cho $patt");
		}
	    } else {
		print "$SELF : Run $run will be ignored. No files returned by ddb\n";
		rdaq_set_message($SSELF,"No files returned by ddb","Run $run will be ignored");
	    }
	}


	# We are done scanning the runs, re-build the CONFF
	# file. Delete lock file afterward.
	open(FO,">$CONFF");
	foreach $line (@RECORDS){ print FO "$line\n";}
	close(FO);
	unlink($LOCKF);

    } else {
	rdaq_set_message($SSELF,"No slots available within range $USEQ[1] / $SPILL[1]","mode=bypass");
    }


} elsif ($TARGET =~ m/(^C\/)(.*)/ ) {
    #
    # AUTO-CALIBRATION MODE
    #
    # Overwrite queue if necessary
    $TST = "Auto-calib mode";
    print "$SELF : FO $TST\n";

    $USEQ[2] = $tmpUQ if ( defined($tmpUQ) );
    $SPILL[2]= $tmpSP if ( defined($tmpSP) );

    # Default mode is submit. Target is a path
    # get the number of possible jobs per queue.
    #print "$SELF : Using $USEQ[2] $SPILL[2]\n";
#    $TOT = CRSQ_getcnt($USEQ[2],$SPILL[2],$PAT,1);

$njobs = 0;
$NSLOT = 0;
$TOT = 0;

    foreach my $jstate (@STATES) {
    $njobs = `/usr/bin/crs_job -long -s $jstate | grep Stdout | grep dev | wc -l` ;
    next if( !defined $njobs) ;
    $NSLOT +=  $njobs;
    }
    $TOT = $MAXSLOTS - $NSLOT;
    if( $TOT < 0) {$TOT = 0};

    print "Number of slots to use 2 ", $TOT, "\n";

#    $TOT = 20;
    $TOT = 1 if ($DEBUG);

    $time = localtime();
    if ($TOT > 0 && ! -e $LOCKF){
	open(FL,">$LOCKF");
	close(FL);
	#print "$SELF : We need to submit $TOT jobs\n";

	# Check $TARGET for sub-mode
	# If $TARGET starts with a ^, take only the top, otherwise
	# go into the 'crawling down the list' mode. We will start
	# with the second mode (gives an idea of how often we have
	# a dead time process earlier runs).

	if( ($obj = rdaq_open_odatabase()) ){
	    if( substr($TARGET,0,1) eq "C" ){
		# Simple with a perl module isn't it.
		$TARGET=~ s/C\//\//;
		undef(@files);
		my($kind);
		foreach $kind ( split(/\|/,$LASERTP) ){
		    push(@files,rdaq_get_ffiles($obj,0,$TOT,$kind));
		}
	    } else {
		# Cannot do this
		print "$SELF : Failed. Wrong syntax\n";
	    }



	    if($#files != -1){
		print "$SELF : Checking ".($#files+1)." jobs\n";
		undef(@OKFILES);             # will be filled by Submit
		undef(@SKIPPED);
		foreach $file (@files){
		    #print "$SELF : HW : $file\n";
###### Submit

		    sleep($SLEEPT) if &Submit(2,$USEQ[2],$SPILL[2],
					      $file,$CHAIN,"Calibration");
		    $MAXCNT--;
		    last if ($MAXCNT == 0);
		}
		rdaq_set_files($obj,5,@OKFILES);  # special flag
		rdaq_set_chain($obj,$SCHAIN,@OKFILES);
		# rdaq_toggle_debug(1);
		# print ">> marking skipped $#SKIPPED $#OKFILES\n";
		#
		# ATTENTION - not counting the real submitted has side
		# effects
		#
		rdaq_set_files($obj,4,@SKIPPED);  # mark skipped
		rdaq_toggle_debug(0);
	    } else {
		# there is nothing to submit
		print "$SELF : There is nothing to submit on $time for $TST\n";
	    }
	    rdaq_close_odatabase($obj);
	}
	if(-e $LOCKF){  unlink($LOCKF);}
    } else {
	print "$SELF : Target=C - There are no slots available within range $USEQ[2] / $SPILL[2]\n";
	rdaq_set_message($SSELF,"No slots available within range $USEQ[2] / $SPILL[2]","mode=calib");
    }

} elsif ($TARGET =~ m/(^X\/)(.*)/ ) {
    #
    # External processing mode
    # TODO: cleanup globals vars
    #
    #print "$SELF : Z processing is being reshaped\n";
    $TST = "Extrenal processing";
    print "$SELF : FO $TST\n";

    $pcount = $#PIPEOFF+1;
    $sanity = int(($pcount)/2)*2;
    if ( $sanity != $pcount ){
	print "$SELF : Configuration error - number of element in PIPEOFF must be even\n";
	exit;
    } else {
	print "$SELF : Configuration is correct, ready to proceed\n";
	# for ($i=0 ; $i <= $#PIPEOFF ; $i++){
	#    if ( int(($i+1)/2) == $i){
	#	push(@PIPEFILES,$PIPEOFF[$i]);
	#    } else {
	#	#push(
	#    }
	# }
	# print "Debug ".join(";",@PIPEFILES)."\n";
	$TOTP = $TOT = 5; # 50;
	# $pick = 0;

	if( ($obj = rdaq_open_odatabase()) ){
	    if( substr($TARGET,0,1) eq "X" ){
		my($user,$i,$kind,$wght);
		my($num);
		my(%Cond);

		foreach $key (keys %XCONDITION){
		    #print "DEBUG We are adding condition for [$key] as $XCONDITION{$key}\n";
		    $Cond{$key} = $XCONDITION{$key};
		}


		$TARGET=~ s/X\//\//;
		($TARGET,$user)=split(":",$TARGET);

		undef(@OKFILES);
		undef(@OKFRMT);
		$cntskipped = $cntreject = 0;

		print "$SELF : $TARGET and user=$user\n";
		#&rdaq_toggle_debug();

		if ( -e "$TARGET/FastOffline-X.conf"){
		    print "$SELF : Configuration $TARGET/FastOffline-X.conf found - reading\n";
		    if ( open(CONF,"$TARGET/FastOffline-X.conf") ){
			while ( defined($line = <CONF>) ){
			    chomp($line);
			    $line =~ s/\#.*//;
			    $line =~ s/^\s*(.*?)\s*$/$1/;
			    next if ($line eq "");
			    if ($line =~ m/(.*)(=)(.*)/){
				$tag = $1;
				$val = $3;
				print "DEBUG - Found in config [$line]\n";
				if ( $tag eq "TOTAL"){
				    $TOTP = $TOT = $val;
				    print "$SELF : TOTAL set to $val by config\n";
				}
			    }
			}
			close(CONF);
		    }
		}
		exit if ($TOTP <= 0);


		#if  ( ! defined($ENV{JobSubmit_GO}) ){
		#    print "Debug - leaving\n";
		#    exit;
		#}

		do {
		    undef(@SKIPPED);
		    undef(@RANDREJECT);
		    undef(@myfiles);
		    undef(@testfls);

		    # ensures at least 1
		    $num = int($TOTP/(($#PIPEOFF+1)/2))+1;		
		    for ($i=0 ; $i <= $#PIPEOFF ; $i++){
			if ( 2*int(($i+1)/2) == $i ){
			    # the kind of streams
			    $kind = $PIPEOFF[$i];
			    #print "Recovering i=$i kind=$kind (pending)\n";
			} else {
			    # the weight of stream to transfer
			    $wght= $PIPEOFF[$i];
			    #print "$SELF : Trying $kind / $wght\n";
			    @files= rdaq_get_files($obj, 0, int($num/$wght*100), 1 , \%Cond, $kind);
			    # ----------------------------^ this will add Status=0 in
			    # conditions - replace by undef otherwise.
			    push(@testfls,@files);
			    push(@myfiles,&RandomPick($wght,@files));
			    print 
				"$SELF : Recovering i=$i kind=$kind picking $num <=> wght=$wght (".
				($#testfls+1)."/".($#files+1)."/".($#myfiles+1)."/".($#MySelection+1)." selected)\n";
			}
		    }

		    # this implies at least one has 100% selection, hence we ensure
		    # we get out with one file
		    if ($#testfls == -1 &&  $#MySelection == -1 ){
			print "$SELF : No file selected (no candidate)\n";
			# Now, we get back to status 9 and 8 and reset it all to zero and try
			# again at the next loop
			undef(%Cond);
			$Cond{"XStatus$ID"} = "8|9";
			#print "$SELF : condition set\n";
			rdaq_toggle_debug(1);
			#print "$SELF : now toggling - calling func\n";
			@files = rdaq_get_orecords($obj,\%Cond, -1);
			print "$SELF : Recoverred ".($#files+1)." records\n";
			rdaq_set_xstatus($obj,$ID,0,@files);
			rdaq_toggle_debug(0);
			exit;
		    } else {
			push(@MySelection,@myfiles);
			rdaq_set_xstatus($obj,$ID,9,@myfiles);
		    }


		    #&rdaq_toggle_debug();
		    $TOTP = $TOT - $#MySelection;

		    # mark right away
		    if ( $#RANDREJECT != -1){
			$cntreject += $#RANDREJECT+1;
			rdaq_set_xstatus($obj,$ID,8,@RANDREJECT);  # mark Random reject
		    }
		    if ( $#SKIPPED != -1 ){
			$cntskipped += $#SKIPPED+1;
			rdaq_set_xstatus($obj,$ID,4,@SKIPPED);     # mark skipped
			rdaq_set_files($obj,4,@SKIPPED);           # mark skipped as well as cond are similar
		    }

		} while ( $#MySelection <  $TOT  && $#testfls != -1);


		#push(@files,rdaq_get_ffiles($obj,0,$TOT,$kind));
		#print "We are considering\n";
		foreach $f (@MySelection){
		    @items = split(" ",$f);
		    #print "\t$items[0]\n";
		    # leverage all logic related to exclusion
####### Submit

		    &Submit(4,0,0,$f,undef,undef);
		}

		#print "We will skip\n";
		#foreach $f (@SKIPPED){
		#    @items = split(" ",$f);
		#    print "\t$items[0]\n";
		#}
		#print "We rejected\n";
		#foreach $f (@RANDREJECT){
		#    @items = split(" ",$f);
		#    print "\t$items[0]\n";
		#}

		# don't bother using the module - generate a file
		# list and submit verbatim
		my($FOUT)="/tmp/JobSubmit-$$.lis";
		open(FDC,">$FOUT");

		if ( $#OKFRMT != -1 ){
		    print "$SELF : We will submit\n";
		    foreach $f (@OKFRMT){
			print "\tWritting out $f\n";
			print FDC "$f\n";
		    }
		    close(FDC);
		    #system("/bin/cat $FOUT");
		    my($TRANSFERCMD)= "/bin/cat $FOUT && /opt/star/bin/hpss_user.pl -i $user -f $FOUT";
		    #print "Would do $TRANSFERCMD\n";
		    system($TRANSFERCMD);
		    $status = $?;
		} else {
		    print "$SELF : Nothing to submit -".
			 " OKFiles=".(1+$#OKFILES).
			 " Skipped=$cntskipped".
			 " Rejected=$cntreject\n";
		    $status = 0; # but this is OK
		}

		if ($status != 0){
		    print "$SELF : Error executing command - please debug $status\n";
		    exit;
		} else {
		    print "$SELF : We will mark any requested files now\n";
		    if ( $#OKFILES != -1){
			rdaq_set_xstatus($obj,$ID,1,@OKFILES);     # mark submitted
			rdaq_set_files($obj,7,@OKFILES);           # set to External
			rdaq_set_chain($obj,"Restored-for-external-processing-on-$target",@OKFILES);
		    }
		}
		unlink($FOUT);

	    } else {
		print "$SELF : Failed. Argument must specify a user and target disk\n";
	    }
	}
    }


} elsif ($TARGET =~ m/(^Z\/)(.*)/ ) {
    #
    # eZTree processing
    #
    # Overwrite queue if necessary
    print "$SELF : Target Z disabled\n";
    exit;


    my(%Cond);

    foreach $key (keys %XCONDITION){
	$Cond{$key} = $XCONDITION{$key};
    }

    # ezTree chain
    if (  $ThisYear == 2004){
	# there was one chain only
	$CHAIN   = "pp2004,ITTF,hitfilt,ezTree,-trg,-Sti,-Ftpc,-SvtD,-fcf,-Corr4";
    } else {
	if ( $DCHAIN{"PPPP"} !~ /ezTree/i){
	    $CHAIN   = $DCHAIN{"PPPP"}.",ezTree,-Sti,-genvtx,-Ftpc,-SvtD,-fcf,-fcl";
	}
    }

    $USEQ[0] = $tmpUQ if ( defined($tmpUQ) );
    $SPILL[0]= $tmpSP if ( defined($tmpSP) );

    # Default mode is submit. Target is a path
    # get the number of possible jobs per queue.
    #print "$SELF : Using $USEQ[1] $SPILL[1]\n";
#    $TOT = CRSQ_getcnt($USEQ[0],$SPILL[0],$PAT,1);

$njobs = 0;
$NSLOT = 0;

    foreach my $jstate (@STATES) {
    $njobs = `/usr/bin/crs_job -long -s $jstate | grep Stdout | grep dev | wc -l` ;
    next if( !defined $njobs) ;
    $NSLOT +=  $njobs;
    }

    print "Number of jobs   ", $NSLOT, "\n";

    $TOT = $MAXSLOTS - $NSLOT;
    if( $TOT < 0) {$TOT = 0};

#    $TOT = 20;
    $TOT = 1 if ($DEBUG);

    print "$SELF : ezTree processing, checking $TOT\n";

    $time = localtime();
    if ($TOT > 0 && ! -e $LOCKF){
	open(FL,">$LOCKF");
	close(FL);
	print "$SELF : We need to submit $TOT jobs\n";

	# Check $TARGET for sub-mode
	# If $TARGET starts with a ^, take only the top, otherwise
	# go into the 'crawling down the list' mode. We will start
	# with the second mode (gives an idea of how often we have
	# a dead time process earlier runs).

	if( ($obj = rdaq_open_odatabase()) ){
	    if( substr($TARGET,0,1) eq "Z" ){
		# Simple with a perl module isn't it.
		$TARGET=~ s/Z\//\//;
		@files = rdaq_get_orecords($obj,\%Cond,$TOT,$COND);
	    } else {
		# Cannot do this
		print "$SELF : Failed. Wrong syntax\n";
	    }



	    if($#files != -1){
		print "$SELF : Checking ".($#files+1)." jobs\n";
		undef(@OKFILES);             # will be filled by Submit
		undef(@SKIPPED);
		foreach $file (@files){
		    #print "$SELF : HW : $file\n";
########## Submit

		    sleep($SLEEPT) if &Submit(1,$USEQ[0],$SPILL[0],
						  $file,$CHAIN,"eZTree");
		    $MAXCNT--;
		    last if ($MAXCNT == 0);
		}
		rdaq_set_xstatus($obj,$ID,1,@OKFILES);  # mark submitted
		rdaq_set_chain($obj,$SCHAIN,@OKFILES);
		rdaq_set_xstatus($obj,$ID,4,@SKIPPED);  # mark skipped
	    } else {
		# there is nothing to submit
		print "$SELF : There is nothing to submit on $time for $TST\n";
	    }
	    rdaq_close_odatabase($obj);
	}
	if(-e $LOCKF){  unlink($LOCKF);}
    } else {
	print "$SELF : Target=Z - There are no slots available within range $USEQ[0] / $SPILL[0]\n";
	rdaq_set_message($SSELF,"No slots available within range $USEQ[0] / $SPILL[0]","Target=XForm");
    }


} elsif ($TARGET == 1) {
    # First argument specified, check job list and results ...
#    CRSQ_check($PAT,"../archive/");

    # clean lock file from default mode. Adds sturdiness
    # the process since the possibility to have a job killed while
    # a lock file was opened is non null.
    if(-e $LOCKF){ unlink($LOCKF);}


} else {
    print "$SELF : Unknown mode P3=$TARGET used\n";
}


#
# Create a job file and submit.
# Note that the full file info is received here ...
#
sub Submit
{
    my($mode,$queue,$spill,$file,$chain,$ident)=@_;
    my($Hfile,$jfile,$mfile,@items);
    my($field,$tags);
    my($trgsn,$trgrs);
    my($filseq);
    my($stagedon);
    my($destination);

    # We are assuming that the return value of $file is
    # the mode 2 of get_ffiles() and counting on the
    # elements position.
    #print "$SELF : $file\n";
    @items   = split(" ",$file);
    $fileseq = $file  = $items[0];
    $coll    = $items[8];

    # this is a patch due to early diffreent coding of collision
    $coll    = "dAu" if ($coll eq "DeuteronAu");

    # get filesequence
    $fileseq =~ s/.*_//; $fileseq =~ s/\..*//;
    $fileseq = int(substr($fileseq,length($fileseq)-4,4));

    # get field as string
    $field   = &rdaq_scaleToString($items[6]);

    # print "$SELF : DEBUG scale=$items[6] --> $field\n"  if ($DEBUG);
    # print "$SELF : DEBUG 10=$items[10] 11=$items[11]\n" if ($DEBUG);

    # Trigger setup string
    $trgsn   = rdaq_trgs2string($items[10]);
    # Triggers  mask information
    $trgrs   = rdaq_bits2string("TrgMask",$items[11]);
    # Detector setup information
    $dets    = rdaq_bits2string("DetSetMask",$items[9]);

    # print "$SELF : DEBUG 10=$trgsn 11=$trgrs\n"          if ($DEBUG);

    if($chain eq "" || $chain eq "none" || $chain eq "default"){
	$chain = $DCHAIN{$coll};

	if( ! defined($chain) ){
	    print
		"$SELF : Warning : ".localtime().
		" No chain options declared. No default for [$coll] either.\n";
	    return 0;
	}

	# check if there is a specific chain for a file match
	foreach $key ( keys %MCHAIN ){
	    my($collision,$nmpat) = split(";",$key);
	    # collision could then be partial
	    if ($coll =~ m/$collision/){
		if ( $file =~ m/$nmpat/){
		    print "$SELF : Info : $file match $nmpat - chain changed for $key\n";
		    $chain = $MCHAIN{$key};
		    last;
		}
	    }
	}

    }

    if($mode == 2){
	# This is the calibration specific mode
	$tags  = "laser";        # **** this is cheap and dirty ****
#        $tags  = "tags";
	$calib = $SCALIB{$coll};
    } else {
	# this is any other mode
	$tags  = "tags";
	$calib = $DCALIB{$coll};
    }
    if( ! defined($calib) ){ $calib = "";}
    if( $mode == 2 && $calib eq ""){
	# Mode to is calibration only so if we are missing
	# the option, do NOT continue.
	push(@SKIPPED,$file);
	print "$SELF : Info : mode 2 requested and calib is empty $file\n";
	return 0;
    } elsif ($calib eq ""){
	# Change it to a dummy value so the
	# soft-link is created.
	#$calib = "DUMMY";
    }

    #
    # ATTENTION - Exclusions
    #
    # Those are explained in the FastOffline documentation.
    #
    # This was added according to an Email I have sent to
    # the period coordinator list. Only Jeff Landgraff
    # has answered saying we can skip the 'test' ones.
    #
    if ( $file =~ /pedestal/){
	print "$SELF : Info : Skipping $file (name matching exclusion on pedestal)\n";
	push(@SKIPPED,$file);
	return 0;

    } elsif ( $trgrs eq "unknown" || $trgsn eq "unknown"){
	print "$SELF : Info : Skipping $file has unknown setup or triggers sn=$trgsn ts=$trgrs\n";
	push(@SKIPPED,$file);
	return 0;

    } elsif ( $trgrs eq "pedestal"     || $trgrs eq "pulser" ||
	      $trgsn eq "pedestal"     || $trgsn eq "pulser" ||
	      $trgsn =~ m/pedAsPhys/i  || $trgrs =~ m/pedAsPhys/i ){
	print 
	    "$SELF : Info : Skipping pulser,pedestal,pedAsPhys in $file, ",
	    "has setup=$trgsn 'triggers'=$items[11]=$trgrs\n";
	rdaq_set_message($SSELF,
			 "Skipped",
			 "Skipping pulser,pedestal,pedAsPhy in $file, has setup=$trgsn 'triggers'=$items[11]=$trgrs");
	push(@SKIPPED,$file);
	return 0;

#    } elsif ( ($trgrs =~ m/test/ || $trgsn =~ m/test/ ||
#	       $trgrs =~ m/tune/ || $trgsn =~ m/tune/   ) && $mode == 0){
    } elsif ( ($trgsn =~ m/test/ || $trgsn =~ m/tune/   ) && $mode == 0){
	if ( $ThisYear == 2002 || $ThisYear >= 2010 ){
	    # start with a warning
	    print "$SELF : Info : Skipping tune,test in $file, has 'triggers'=$items[11]=$trgrs\n";
	    rdaq_set_message($SSELF,
			     "Skipped",
			     "Skipping tune,test in $file, has 'triggers'=$items[11]=$trgrs");
	    push(@SKIPPED,$file);
	    return 0;
	} else {
	    print
		"$SELF : Info :: $file has 'triggers'=$items[11]=$trgrs ",
		"but Year=$ThisYear not skipping it\n";
	    rdaq_set_message($SSELF,"Skipped",
		"Not skipping $file has 'triggers'=$items[11]=$trgrs (contains tune or test), YEar=$ThisYear");
	}
    }

    # just for one request
    #if ($file !~ m/st_physics/ && $file !~/st_zerobias/){
    #if ($file !~ m/st_zerobias/){
    #	print "$SELF : Info : Skipping $file (special hardcoded exclusion)\n";
    #	push(@SKIPPED,$file);
    #	return 0;
    #}


    # Note that skipping dets when tpc is not present is ONLY related to
    # mode 1. While mode is weakly related to regular/calib/bypass, mode Z (ezTree)
    # uses mode=1 and will therefore ACCEPT files with no tpc information in.
    if ( $dets ne "tpc" && $dets !~ m/\.tpc/ &&  $dets !~ m/tpc\./ &&
	 $dets ne "tpx" && $dets !~ m/\.tpx/ &&  $dets !~ m/tpx\./){
	if ($mode != 1){
	    print "$SELF : Info : detectors are [$dets] (not including tpc) skipping $file\n";
	    rdaq_set_message($SSELF,"Skipped","detectors=$dets (not including tpc) for $file");
	    push(@SKIPPED,$file);
	    return 0;
	} else {
	    print "$SELF : Info : detectors are [$dets] (not including tpc) - Submitting anyway\n";
	}
    }


    # ------------ selectionn/ exclusion logic is done --------------


    # Last element will always be the Status
    # Even in ezTree mode or else, this should remain
    if($items[$#items] != 0 ){
	print "$SELF : Found status = $items[$#items] (??)\n";
	return;
    }

    # Otherwise, we do have a valid entry
    $Hfile = rdaq_file2hpss($file,3);
    @items = split(" ",$Hfile);


    # No trigger information nowadays
    $m     = sprintf("%2.2d",$items[3]);
    $dm    = $items[4];
    $jfile = join("_",$LIB,$items[2],$m,$file);
    $jfile =~ s/\..*//;
    $mfile = $file;
    $mfile =~ s/\..*//;

    # transfer - only leverage logic and push into @OKFILES global array
    if ( $mode == 4){
	my(@el)=split(" ",$Hfile);
	$dfile = $el[0];
	$dfile =~ s/\/home\/starsink\/raw/$TARGET/;
	push(@OKFILES,$file);
	push(@OKFRMT,"$el[0]/$el[1] $dfile/$el[1]");
	return;
    }



    # Just in case ...
    if( -e "$jfile"){
	print "$SELF : Info : $jfile exists. Ignoring for now\n";
	return 0;
    }

    # THIS IS HACK FOR 2004 data until Jeff fixes the names
    $prefix = "";
    #$prefix = "COPY_";

    # PATH are different depending on HPSS storage or local
    # Mode for UNIX is a Fast-local buffering.
    if ( $TREEMODE == 0){
	$XXX = "$LIB/$items[2]/$m";
    } else {
	if ($ThisYear < 2006){
	    $XXX = "$trgsn/$field/$LIB/$items[2]/$dm";
	} else {
	    if ( $mfile =~ m/(\d+)(_raw_)/ ){
		$run = $1;
	    } else {
		$run = 0;
	    }
	    $XXX   = "$trgsn/$field/$LIB/$items[2]/$dm/$run";
	}
    }

    if ($HPSS){
	$SCRATCH     = "/home/starreco/reco/$XXX";
	$destination = "$TARGET";  $destination =~ s/\/reco//;
	$stagedon    = "HPSS";
    } else {
	$SCRATCH     = ".";
	$destination = "$TARGET/$XXX";
	$stagedon    = "UNIX";
    }

my $NEVT = $MAXEVT!=0?$MAXEVT:$NUMEVT ;

    # Now generate the file and submit
    if( open(FO,">$jfile") ){
	if($calib ne ""){ 

	    $chain = "LanaDVtpx,ITTF,CorrX,OSpaceZ2,OGridLeak3D";
	    # ------------------------------------------------------------------
	    # THIS IS A CALIBRATION PRE-PASS -- IT REQUIRES AN ADDITIONAL INPUT
	    print FO <<__EOF__;

[output-0]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.laser.root

[exec-0]
args = 4  $LIBV $destination $NEVT $chain
gzip_output = True
stdout = $DSKLOG/prodlog/$LIB/log/daq/$mfile.log
stderr = $DSKLOG/prodlog/$LIB/log/daq/$mfile.err 
exec = $SPATH/bfcca

[main]
num_inputs = 1
num_outputs = 1
queue = highest

[input-0]
path = $items[0]
type = HPSS
file = $prefix$items[1]

__EOF__

        } else {
	    # THIS IS A REGULAR RECONSTRUCTION PROCESSING
#	    print FO <<__EOH__;

#[input-0]
#path = $items[0]
#type = HPSS
#file = $prefix$items[1]

#__EOH__
     }

	    # ------------------------------------------------------------------

	# SEVERAL OUTPUT "MAY" BE CREATED, NOTE THAT IN CALIBF MODE, $tags WILL
	# BE CHANGED TO TAKE INTO ACCOUNT THE laser.root FILE.

	# first, decide if event.root should be saved - once every 10 ...
	$AllFiles = ($fileseq % 10 == 0 && $fileseq !=0) || $fileseq ==1;
	if ( $AllFiles){
	    $hint = "filseq check lead to use event.root";

	    # ... and do regular MC i.e. toss a coin
	    my($testf)=lc("$prefix$items[1]");
	    if ( $testf =~ m/(st_)(\w+)(_\d+_)/ ){  # should match always
		$testf = $2;
		if ( $SKIPMC =~ m/$testf/ ){
		    $hint = "[$testf] matches $SKIPMC - enabling all event.root";
		} else {
		    $prob = int(rand()*100);
		    if ($prob >= $FRACTT ){
			# set to false
			$AllFiles = 1==0;
			$hint = "filseq check -> event.root but disabled by regular MC ($prob >=  $FRACTT)";
		    }
		} 

	    } else {
		$hint = "[$testf] did not match pattern";
	    }
	} else {
	    $hint = "standard file selection";
	}

	if ( $AllFiles ){

    if($calib ne ""){ goto GO_CLOSE; }

	    print FO <<__EOF__;

[output-0]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.event.root

[output-1]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.MuDst.root

[output-2]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.hist.root      

[output-3]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.tags.root     

[exec-0]
args = 4  $LIBV $destination $NEVT $chain
gzip_output = True
stdout = $DSKLOG/prodlog/$LIB/log/daq/$mfile.log
stderr = $DSKLOG/prodlog/$LIB/log/daq/$mfile.err 
exec = $SPATH/bfcca

[main]
num_inputs = 1
num_outputs = 4
queue = highest

[input-0]
path = $items[0]
type = HPSS
file = $prefix$items[1]

__EOF__

	} else {

    if($calib ne ""){ goto GO_CLOSE; }

	    print FO <<__EOF__;

[output-0]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.MuDst.root

[output-1]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.hist.root

[output-2]
path = $SCRATCH
type = $stagedon
file = $prefix$mfile.tags.root      

[exec-0]
args = 4  $LIBV $destination $NEVT $chain
gzip_output = True
stdout = $DSKLOG/prodlog/$LIB/log/daq/$mfile.log
stderr = $DSKLOG/prodlog/$LIB/log/daq/$mfile.err 
exec = $SPATH/bfcca

[main]
num_inputs = 1
num_outputs = 3
queue = highest

[input-0]
path = $items[0]
type = HPSS
file = $prefix$items[1]

__EOF__

 GO_CLOSE:

    }

	close(FO);

	# A returned value
	$SCHAIN = $chain;

	if( (stat($jfile))[7] == 0){
	    print "$SELF : Info : 0 size $jfile . Please, check quota/disk space\n";
	    unlink($jfile);
	    return 0;
	} else {
	    if ( ! $DEBUG ){
#		if ( CRSQ_submit($jfile,$PRIORITY,$queue,$spill) ){
                `/usr/bin/crs_job -insert $jfile`;
                `/bin/mv $jfile ../archive`; 
		    # Mark it so we can set status 1 later
#		    print "$SELF : Successful submission of $file ($queue,$spill) on ".
			localtime()."\n";

		    rdaq_set_execdate($obj,undef,$file);  # set execdate, more or less meaning submit
		    rdaq_set_message($SSELF,"Submitted",$file);
		    push(@OKFILES,$file);
		    return 1;
#		}
	    } else {
		rdaq_set_message($SSELF,"Notice","DEBUG is ON - There will be no submission");
		print "$SELF : DEBUG is on, $jfile not submitted\n";
		return 0;
	    }
	}
    } else {
	print "$SELF : Fatal : Could not open $jfile\n";
	rdaq_set_message($SSELF,"Fatal","Could not open $jfile for write");
	return 0;
    }

}

#
# This routine was written to randomize a file list.
# Did not turn out to be usefull actually so ... but
# left anyhow in case.
#
sub Scramble
{
    my(@files)=@_;
    my(@TMP,$i,$s);

    #print "Scrambling ...\n";
    while ($#files != -1){
	$i = rand($#files+1);
	$s = splice(@files,$i,1); #print "Removing $s\n";
	push(@TMP,$s);
    }
    return @TMP;
}

sub RandomPick
{
    my($prob,@files)=@_;
    my(@TMP,$p);

    # special cases
    if ($prob == 100){  return @files;}
    if ($prob == 0)  {  return undef;}

    foreach $f (@files){
	$p = int(rand(100)+1);
	if ($p < $prob){  push(@TMP,$f);}         # <-- local
	else {            push(@RANDREJECT,$f);}  # <-- global
    }
    return @TMP;
}

#
# Act on array itself (will be faster) hence receive a
# reference to an array (do not get confused).
#
# This implements the Fisher-Yates technique. I use splicing
# abilities of perl for the permunation (no need for an intermediate
# var).
#
sub RandArray
{
    my($array) = @_;
    my($i,$j);
    for ($i = @$array; --$i; ) {
	last if ($i == -1);
        $j = int( rand($i+1));
        next if ($i == $j);
        @$array[$i,$j] = @$array[$j,$i];
    }
}



#
# Global xit routine in case cleanups are needed
#
sub Exit
{
    my($mess)=@_;

    if ( defined($mess) && defined($ENV{JS_DEBUG}) ){
	print "$mess\n";
    }
    exit;
}
