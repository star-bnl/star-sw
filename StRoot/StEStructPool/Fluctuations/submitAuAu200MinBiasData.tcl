#!/bin/sh
#-*-tcl-*-
# the next line restarts using tclsh \
exec tclsh "$0" -- ${1+"$@"}

# Proc to define a generic cuts file.
# A few cuts are not relevant to Hijing.
#>>>>> Customize this in the proc.
#      Note that Pt and centrality/numTracks cuts overlap with
#      cuts in the macro file.
#      Eta cuts overlap with cuts in source code.
proc genericCutsFile {} {
    return {# ******************************************** 
# *************** Event Cuts ***************** 
# *** format = variable,minvalue,maxvalue  *** 
# ******************************************** 

triggerWord,AuAu200GeVMinBias2001   # triggerWord cut (AuAu minBias
primaryVertexZ,-15,15               # primary vertex cut
centrality,1,1500                   # centrality cut
numTracks,1,1500                    # number of tracks per event passing track cuts
# ******************************************** 
# ******************************************** 
# *************** Track Cuts ***************** 
# *** format = variable,minvalue,maxvalue  *** 
# ******************************************** 

Flag,0,2000                # track flag cut
Charge,-1,1                 # charge cut
NFitPoints,15,50            # fit points cut
NFitPerNMax,0.53,1.0        # fitpoints per possible cut
GlobalDCA,0.,3.0            # global DCA cut
Chi2,0.,99.0                # chi square cut
Pt,0.15,2.0                 # pt cut
Phi,-1,1                    # phi cut
Eta,-1,1                    # eta cut
#NSigmaElectron,-10,10      # num sigma electron cut
#NSigmaPion,-10,10          # num sigma Pion cut
#NSigmaKaon,-10,10          # num sigma Kaon cut
#NSigmaProton,-10,10        # num sigma proton cut

# ******************************************** 
# ******************************************** 
# ******************************************** 
# *************** Pair Cuts ***************** 

#DeltaPhi,0.,2.
#DeltaEta,0.,2.25
#DeltaMt,0.,500000.0
#qInv,0.0,500000.
#EntranceSep,0.0,200
#ExitSep,0.0,650
#Quality,-0.5,1.0
#MidTpcSepLikeSign,0.0,400.
#MidTpcSepUnlikeSign,0.0,400.
    }
}
#<<<<< End of customization

# Proc to define a generic macro file.
# Centrality selection depends on system.
# Customize these in the proc.
proc genericMacroFile {} {
    return {void doEstruct( const char* fileListFile,
                            const char* outputDir,
                            int maxNumEvents = 1000 ) {

            const char* jobName = "temp";
            const char* cutFile = "SCRIPTDIR/CutsFile.txt";

            gROOT->LoadMacro("load2ptLibs.C");
            load2ptLibs();

            gROOT->LoadMacro("getOutFileName.C");

            // simple (global) centrality definition ...not persistant to event file.
            //>>>>> Here we need to customize number and value of
            //      multiplicity/pt/impact parameter cuts.
            StEStructCentrality* cent=StEStructCentrality::Instance();
            // Following line is using standard reference multiplicity.
            // const double mbBins[] = { 3,  7, 14, 30,  56,  94, 146, 217, 312, 431, 510,  800 };
            // Following line is using all tracks passing cuts.
             const double mbBins[] = { 7, 18, 38, 71, 121, 192, 289, 415, 575, 770, 887, 1200 };
            cent->setCentralities(mbBins,12);
            const  double ptCut[] = { 0.15, 0.5, 2.0};
            const  double ptMultCut[] = { 18, 289, 1200 };
            cent->setPts(ptCut,3,ptMultCut,3);

            // Histogram centrality/pt selection so we can include them
            // in the data histogram files.
            TH1D *cenClass = new TH1D("cenClass","cenClass",13,-1.5,12.5);
            TH1D *impactDefs   = new TH1D("impactDefs","impactDefs",12,0.5,12.5);
            for (int i=0;i<7;i++) {
                impactDefs->Fill(i,mbBins[i]);
            }
            TH1D *ptRanges     = new TH1D("ptRanges","ptRanges",3,0.5,3.5);
            for (int i=0;i<3;i++) {
                ptRanges->Fill(i,ptCut[i]);
            }
            TH1D *impactPtDefs = new TH1D("impactPtDefs","impactPtDefs",3,0.5,3.5);
            for (int i=0;i<3;i++) {
                impactPtDefs->Fill(i,ptMultCut[i]);
            }
            //>>>>> End of customization


            StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",500); 

            // now Set up the Maker
            StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStructFluct");

            // Set up the EStructDstReaders and Analysis codes
            StEStructFluctAnal *analysis = new StEStructFluctAnal(4);
            analysis->setCentralityObject(cent);
            analysis->setCutFile(cutFile);

            StEStructEventCuts   *ecuts   = new StEStructEventCuts(cutFile);
            StEStructTrackCuts   *tcuts   = new StEStructTrackCuts(cutFile);
            StEStructMuDstReader *readers = new StEStructMuDstReader(mk,ecuts,tcuts,false);

            estructMaker->SetReaderAnalysisPair(readers,analysis);


            int istat=0, i=0;
            estructMaker->Init();
            estructMaker->startTimer();

            int counter=0;
            while (istat!=2) {

                istat=estructMaker->Make();

                if (analysis->mCurrentEvent) {
                    cenClass->Fill(analysis->mCurrentEvent->Centrality());
                }

                i++; counter++;
                if (counter==1000) {
                    cout<<"doing event ="<<i<<endl;
                    counter=0;
                }
                if ( maxNumEvents!=0 && i>=maxNumEvents ) {
                    istat=2;
                }
            }
            estructMaker->stopTimer();
            cout<<endl;
            estructMaker->logAnalysisTime(cout);
            estructMaker->logInputEvents(cout);
            estructMaker->logOutputEvents(cout);
            estructMaker->logOutputRate(cout);

            // --> cuts stats streamed to stdout 
            //
            ecuts->printCuts(cout);
            tcuts->printCuts(cout);
            StEStructPairCuts& pcuts=analysis->getPairCuts();
            pcuts.printCuts(cout);

            // Write histograms used for further analysis
            char *outputFile = getOutFileName(outputDir,jobName,"data");
            TFile *tfd = new TFile(outputFile,"RECREATE");
            analysis->writeHistograms(tfd);
            impactDefs->Write();
            ptRanges->Write();
            impactPtDefs->Write();
            tfd->Close();

            // 
            // --> root cut file
            // 
            char  *rootCutFile = getOutFileName(outputDir,jobName,"cuts");
            TFile *tfc = new TFile(rootCutFile,"RECREATE");
            ecuts->writeCutHists(tfc);
            tcuts->writeCutHists(tfc);
            pcuts.writeCutHists(tfc);
            tfc->Close();

            // 
            // --> root QA file
            // 
            char  *rootQAFile=getOutFileName(outputDir,jobName,"QA");
            TFile *tfq = new TFile(rootQAFile,"RECREATE");
            analysis->writeQAHists(tfq);
            cenClass->Write();
            tfq->Close();

            estructMaker->Finish();
        }
    }
}

proc genericXmlFile {} {
    return {<?xml version="1.0" encoding="utf-8" ?>
<job maxFilesPerProcess="200">
<command>
    cd SUBMITTINGDIR
    starver SL03h
    root4star -q -b SCRIPTDIR/doEstruct.C\(\"SCRIPTDIR/$FILELIST\",\"$SCRATCH\",0\)
</command>
<stdout URL="file:LOGDIR/$JOBID.out"/>
<input URL="catalog:star.bnl.gov?production=P02ge,collision=AuAu200,trgsetupname=MinBiasVertex,filetype=daq_reco_MuDst,sanity=1,storage!=HPSS,magscale=FullField" singleCopy="true" preferStorage="local" nFiles="all" />
<input URL="catalog:star.bnl.gov?production=P02ge,collision=AuAu200,trgsetupname=MinBiasVertex,filetype=daq_reco_MuDst,sanity=1,storage!=HPSS,magscale=ReversedFullField" singleCopy="true" preferStorage="local" nFiles="all" />
<output fromScratch="temp/data/*.root" toURL="file:LOGDIR/data/" />
<output fromScratch="temp/cuts/*.root" toURL="file:LOGDIR/cuts/" />
<output fromScratch="temp/QA/*.root"   toURL="file:LOGDIR/QA/" />
</job>
    }
}

# Check that all directories exist. Make them if they don't.
set path $env(MYDATA)/Data/auau200/minbias
if {[file pathtype $path] == "relative"} {
    set d [list]
} elseif {[file pathtype $path] == "absolute"} {
    set d [list /]
}
foreach dir [split $path /] {
    set d [file join $d $dir]
    if {![file exists $d]} {
        file mkdir $d
    }
}
if {![file exists $path/QA]} {
    file mkdir $path/QA
}
if {![file exists $path/cuts]} {
    file mkdir $path/cuts
}
if {![file exists $path/data]} {
    file mkdir $path/data
}
if {![file exists $path/scripts]} {
    file mkdir $path/scripts
}

# Create cuts file for submitting job.
set f   [open $path/scripts/CutsFile.txt w]
regsub -all SCRIPTDIR [genericCutsFile] $path/scripts cuts
puts $f $cuts
close $f

# Create macro file for submitting job.
set f   [open $path/scripts/doEstruct.C w]
regsub -all SCRIPTDIR [genericMacroFile] $path/scripts macro
puts $f $macro
close $f

# Create xml file for submitting job.
set f   [open $path/scripts/auau200MinBiasData[pid].xml w]
regsub -all SCRIPTDIR    [genericXmlFile] $path/scripts xml
regsub -all LOGDIR        $xml            $path         xml2
regsub -all SUBMITTINGDIR $xml2           [pwd]         xml3
puts $f $xml3
close $f

# Copy this script to the output file.
set s [info script]
file copy -force $s $path/jobSubmissionScript.tcl


# Now go to our job submission directory and invoke the scheduler.
set curr [pwd]
cd $path/scripts
puts "About to invoke scheduler. This may take a while."
catch {exec star-submit auau200MinBiasData[pid].xml} result
set  f  [open $path/jobSubmissionCommand.log w]
puts $f $result
close $f
cd $curr

# On pdsf we run use_stardata.pl to change the submitted jobs
# from running on particular node to requiring a particular data set.
set machine [exec uname -n]
if {[string first pdsf $machine] >= 0} {
    exec use_stardata.pl
}

