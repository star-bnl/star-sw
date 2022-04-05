#!/usr/bin/perl
# Updated to conform to input/output issues in mid/late 2013 -Kevin Adkins
$runList="runs.txt";
$NFiles="\"all\"";
$schedDir="/gpfs01/star/i_uky/jkadkins/bemcTrees2012/sched/";
$outDir="/gpfs01/star/subsysg/EMC2/bemcTrees2012/pp200Trees/";
$logDir="/gpfs01/star/i_uky/jkadkins/bemcTrees2012/logs/";
$xmlDir="/gpfs01/star/i_uky/jkadkins/bemcTrees2012/xml/";

open(FILE,$runList); 
@runs=<FILE>;
$nRuns=$#runs;
print "$nRuns in $runList \n";
for($i=0;$i<=$nRuns;$i++){
    chomp($runs[$i]);
    print "$i: run = $runs[$i]\n";
    print "Writing xml file for Run $runs[$i]:\n";
    open (OUTFILE, "> $xmlDir/mysched_$runs[$i]_$i.xml");
    print(OUTFILE "<?xml version=\"1.0\" encoding=\"utf-8\" ?> \n");
    print(OUTFILE "<job name=\"bemcTrees\" filesPerHour=\"5\" minFilesPerProcess=\"1\" maxFilesPerProcess=\"15\" fileListSyntax = \"xrootd\" simulateSubmission = \"false\"> \n");
    print(OUTFILE "<command>  \n");
    print(OUTFILE "root4star -b -q bemcCalibMacro.C\\\(\\\"bemcCalibTree_$runs[$i]_\$JOBINDEX.root\\\"\,\\\"\$FILELIST\\\"\,\$INPUTFILECOUNT\,1e8\\\) &gt; &amp; bemcLog_$runs[$i]_\$JOBINDEX.CodeOutput.log \n");
    print(OUTFILE "</command> \n");
    print(OUTFILE "<Generator> \n");
    print(OUTFILE "<Location>$schedDir</Location> \n");
    print(OUTFILE "</Generator> \n");

    print(OUTFILE "<SandBox installer=\"ZIP\">\n");
    print(OUTFILE "<Package name=\"bemcCalibCode2012\">\n");
    print(OUTFILE "<File>file:/star/u/jkadkins/bemcCalib2012/StRoot/StEmcPool/StEmcOfflineCalibrationMaker/macros/bemcCalibMacro.C </File> \n");
    print(OUTFILE "<File>file:/star/u/jkadkins/bemcCalib2012/.sl64_gcc447 </File> \n");
    print(OUTFILE "</Package>\n");
    print(OUTFILE "</SandBox>\n");

    print(OUTFILE "<stdout URL=\"file:$logDir/output_$runs[$i]_\$JOBINDEX.log\"/> \n");
    print(OUTFILE "<stderr URL=\"file:$logDir/output_$runs[$i]_\$JOBINDEX.err\"/>  \n");
    print(OUTFILE "<output fromScratch=\"*.root\" toURL=\"file:$outDir\" /> \n");
    print(OUTFILE "<output fromScratch=\"*.log\" toURL=\"file:$logDir\" /> \n");
    print(OUTFILE "<input URL=\"catalog:star.bnl.gov?production=P12id,filetype=daq_reco_MuDst,sanity=1,trgsetupname=pp200_production_2012,filename~st_physics,runnumber=$runs[$i],tpx=1,emc=1,storage!=HPSS\" nFiles=$NFiles />  \n");

    print(OUTFILE "</job> \n");

    close (OUTFILE);

    system "cd $xmlDir";
    print "Submitting jobs to Condor for Run $runs[$i]:\n";
    system "star-submit $xmlDir/mysched_$runs[$i]_$i.xml";
}
