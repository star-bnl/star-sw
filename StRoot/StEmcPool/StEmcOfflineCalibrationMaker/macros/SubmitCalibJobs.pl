#!/opt/star/sl44_gcc346/bin/perl
$runList="runlist.txt";
#$NFiles="\"5\"";
$NFiles="\"all\"";
$workingDir="/star/u/aohlson/bemcCalib2009/";
$schedDir="/star/u/aohlson/bemcCalib2009/SchedDir/";
$outDir="/star/data05/scratch/aohlson/bemcTrees2009/trees/";
#$outDir="/star/data07/EMC2/bemcTrees2011/";
$logDir="/star/data05/scratch/aohlson/bemcTrees2009/logs/";
$scriptDir="/star/data05/scratch/aohlson/bemcTrees2009/scripts/";

open(FILE,$runList); 
@runs=<FILE>;
$nRuns=$#runs;
print "$nRuns in $runList \n";
for($i=0;$i<=$nRuns;$i++){
#for($i=0;$i<=1;$i++){
    chomp($runs[$i]);
 print "$i: run = $runs[$i]\n";

 print "Writing xml file for Run $runs[$i]:\n";
 open (OUTFILE, "> $schedDir/mysched_$runs[$i]_$i.xml");
 print(OUTFILE "<?xml version=\"1.0\" encoding=\"utf-8\" ?> \n");
 print(OUTFILE "<job name=\"bemcTrees\" filesPerHour=\"20\" minFilesPerProcess=\"1\" maxFilesPerProcess=\"20\" fileListSyntax = \"xrootd\" simulateSubmission = \"false\"> \n");
 print(OUTFILE "<command>  \n");
 print(OUTFILE "cd $workingDir \n");
 #print(OUTFILE "root4star -q -b bemcCalibMacro.C\\\(\"500000\"\\\,\\\"\$FILELIST\\\"\\\,\\\"\"MuDst\"\\\"\\\,\\\"\"$outDir/picoDstAuAu07MB_$runs[$i]_\$JOBID.root\"\\\"\\\) \n");
  print(OUTFILE "root4star -b -q bemcCalibMacro.C\\\(\\\"\$SCRATCH\\\"\,\\\"bemctree_$runs[$i]_\$JOBID.root\\\"\,\\\"\$FILELIST\\\"\,\$INPUTFILECOUNT\,2000000\\\) \n");
 print(OUTFILE "</command> \n");
 print(OUTFILE "<Generator> \n");
 print(OUTFILE "<Location>$scriptDir</Location> \n");
 print(OUTFILE "</Generator> \n");
 print(OUTFILE "<stdout URL=\"file:$logDir/output_$runs[$i]_\$JOBID.log\"/> \n");
 print(OUTFILE "<stderr URL=\"file:$logDir/output_$runs[$i]_\$JOBID.err\"/>  \n");
 print(OUTFILE "<output fromScratch=\"*.root\" toURL=\"file:$outDir\" /> \n");

 print(OUTFILE "<input URL=\"catalog:star.bnl.gov?production=P11id,filetype=daq_reco_MuDst,sanity=1,trgsetupname=production2009_200GeV_Single,filename~st_physics,runnumber=$runs[$i],tpx=1,emc=1,storage!=HPSS\" nFiles=$NFiles />  \n");
 #print(OUTFILE "<input URL=\"catalog:star.bnl.gov?production=P10ic,filetype=daq_reco_MuDst,sanity=1,trgsetupname=production2009_200GeV_Single,filename~st_physics,runnumber=$runs[$i],tpx=1,emc=1,storage!=HPSS\" nFiles=$NFiles />  \n");


 print(OUTFILE "</job> \n");

 close (OUTFILE);

 system "cd $schedDir";
 print "Submitting jobs to Condor for Run $runs[$i]:\n";
 system "star-submit $schedDir/mysched_$runs[$i]_$i.xml";
}
