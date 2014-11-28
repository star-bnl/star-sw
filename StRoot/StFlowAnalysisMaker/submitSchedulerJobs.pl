#! /opt/star/bin/perl -w

use File::Basename;
use Getopt::Std;
use Cwd 'abs_path';     # aka realpath()

my %opt; 
getopts('htoz:b:m:adn:q:x:y:s',\%opt);
my $usage = "$0  configfile combinedDenominatorDir\n";
$usage .= "\t if no combinedDenominatorDir, assuming it is the first time submition \n";
#----

my $configFile = shift or die $usage;
my $combinedDenomDir = shift;

$configFile =~ /.config/ or die "are you sure that $configFile is a config file ? \n";

#my $pwd = abs_path ( $ENV { 'PWD' } );
my $pwd = `pwd`;
if ($pwd=~/direct/) { #if at RCF, /direct/star+dataxx/ does not work ! ANNOYING
$pwd=~ s/\/direct//;
$pwd=~ s/\+/\//;   # change /direct/star+dataxx to /star/dataxx
       }
chomp($pwd);


if ($combinedDenomDir) {
chomp($combinedDenomDir);
-e $pwd."/$combinedDenomDir" or die "$combinedDenomDir does not exit ! ";
}



# read in config file
  open (CONFIG, "< $configFile") or die "can't open $configFile $!";
  while (<CONFIG>){
       chomp;                          # no newline
       s/#.*//;                        # no comments
       s/^\s+//;                       # no leading white
       s/\s+$//;                       # no trailing white
       next unless length;             # anything left?
       my ($var, $value) = split (/\s*:\s*/, $_,2);
       $ConfigMap{$var} = $value;
     }
  close(CONFIG);

@flowCents = (
# "cen0",
# "cen1",
# "cen2",
# "cen3",
 "cen4"
# "cen5",
# "cen6",
# "cen7",
# "cen8",
# "cen9"
);


@combinedFiles = (
 "denominator.root"
 );



my $macroName        = $ConfigMap{"macroName"};
my $libVersion       = $ConfigMap{"libVersion"};
my $runJob           = $ConfigMap{"scriptName"};
my $jobPrefix        = $ConfigMap{"partnPrefix"};
my $picoDstLink      = $ConfigMap{"picoDSTLink"};
my $NPartition       = $ConfigMap{"NPartition"};
my $jobDir           = $ConfigMap{"jobDir"};
my $jobName          = $ConfigMap{"jobName"};
my $dataDir          = $ConfigMap{"dataDir"};
my $logFile          = $ConfigMap{"logFileName"};
my $cumulFile        = $ConfigMap{"histoFile"};
my $yieldFile        = $ConfigMap{"yieldFile"};
my $meanGFile        = $ConfigMap{"meanGFile"};
my $weightFile       = $ConfigMap{"weightFile"};
my $schedName        = $ConfigMap{"schedName"};
my $maxFilesPerJob   = $ConfigMap{"maxFiles"};

# move the previouse run as old
if ($combinedDenomDir) {
    if (-e "$jobDir/$jobName"){
 print `mv $jobDir/$jobName   $jobDir/$jobName."old"`;
    }
}

# make job directories
 print `mkdir $jobDir/$jobName`;

# copying source code to $jobName
print `cp -Lr $pwd/StRoot $jobDir/$jobName`;

if (-e "$pwd/PIDTable.root") {
print `cp -Lr $pwd/PIDTable.root $jobDir/$jobName`;
}


print `cp $configFile $jobDir/$jobName/$configFile`;

# cons
 my $consFileName="consDir";
 open ( consScript, ">".$consFileName.".csh");
 print consScript  "#!/bin/csh \n";
 print consScript  "cd  $jobDir/$jobName \n";
 print consScript  "source \$GROUP_DIR/.starver $libVersion \n";
 print consScript  "cons \n";
 close consScript;

 print "$consFileName.csh has been generated \n";
 print "now run $consFileName.csh  : \n";
 print `csh -x ./$consFileName.csh`;
 print `rm ./$consFileName.csh`;


# making centrality directories, and lib links
 foreach $cent(@flowCents) {
 print `mkdir $jobDir/$jobName/$cent`;
 if(-e "$jobDir/$cent.flowPhiWgtNew.hist.root") {
 print `cp $jobDir/$cent.flowPhiWgtNew.hist.root $jobDir/$jobName/$cent/flowPhiWgt.hist.root \n`;
 }
 if(-e "$jobDir/zdcsmdConstants.root") {
 print `cp $jobDir/zdcsmdConstants.root $jobDir/$jobName/$cent/ \n`;
 }
 print `ln -s $jobDir/$jobName/.sl302_gcc323 $jobDir/$jobName/$cent/.sl302_gcc323 \n`;
# print `ln -s $jobDir/$jobName/.rh80_gcc32 $jobDir/$jobName/$cent/.rh80_gcc32 \n`;
# print `ln -s $jobDir/$jobName/.share  $jobDir/$jobName/$cent/.share   \n`;
# print `ln -s $jobDir/$jobName/.i386_linux24  $jobDir/$jobName/$cent/.i386_linux24   \n`;

 if (-e "$jobDir/$jobName/PIDTable.root"){
 print `ln -s $jobDir/$jobName/PIDTable.root  $jobDir/$jobName/$cent/PIDTable.root   \n`;
}

 if ($combinedDenomDir) {

 foreach $combinedFile (@combinedFiles){
 print `cp $pwd/$combinedDenomDir/$cent/$combinedFile $jobDir/$jobName/$cent/$combinedFile`;
 }
}

# producing macro
   open (macroFile,">$jobDir/$jobName/$cent/$macroName");
   open (prototypeMacro,$pwd."/doFlowEvents.C");

   while ($eachLine = <prototypeMacro>) {

     if ($eachLine=~/SetCent/) {
    if ($cent=~/cen0/){$eachLine="    StFlowCutEvent::SetCent(0, 0 ); \n";}
    if ($cent=~/cen1/){$eachLine="    StFlowCutEvent::SetCent(1, 1); \n";}
    if ($cent=~/cen2/){$eachLine="    StFlowCutEvent::SetCent(2, 2); \n";}
    if ($cent=~/cen3/){$eachLine="    StFlowCutEvent::SetCent(3, 3); \n";}
    if ($cent=~/cen4/){$eachLine="    StFlowCutEvent::SetCent(4, 4); \n";}
    if ($cent=~/cen5/){$eachLine="    StFlowCutEvent::SetCent(5, 5); \n";}
    if ($cent=~/cen6/){$eachLine="    StFlowCutEvent::SetCent(6, 6); \n";}
    if ($cent=~/cen7/){$eachLine="    StFlowCutEvent::SetCent(7, 7); \n";}
    if ($cent=~/cen8/){$eachLine="    StFlowCutEvent::SetCent(8, 8); \n";}
    if ($cent=~/cen9/){$eachLine="    StFlowCutEvent::SetCent(9, 9); \n";}
     }

     print macroFile $eachLine;
 }

   close macroFile;
   close prototypeMacro;


# producing shell script

 open (shellScript,">$jobDir/$jobName/$cent/$runJob");
  print shellScript "#! /bin/csh \n";
  print shellScript "set logfile=$logFile \n";
  print shellScript "set prototypeWDIR=$jobDir/$jobName/$cent \n";
  print shellScript "\n";
  print shellScript "set WDIR=/tmp/aihong/\$JOBID \n"; # a temporary LOCAL working dir 
  print shellScript " \n";
  print shellScript "if ( -e \"/tmp/aihong/\$JOBID \" ) then \n";
  print shellScript "rm -rf /tmp/aihong/\$JOBID \n";
  print shellScript "endif \n";
  print shellScript " \n";

  print shellScript "mkdir -p \$WDIR || exit 1 \n";
  print shellScript "cd \$WDIR \n";

  print shellScript "if ( -e \"\$prototypeWDIR/denominator.root\" ) then \n";
  print shellScript "cp -Lr \$prototypeWDIR/denominator.root . \n";
  print shellScript "endif \n";

  print shellScript "if ( -e \"\$prototypeWDIR/doFlowEvents.C\" ) then \n";
  print shellScript "cp -Lr \$prototypeWDIR/doFlowEvents.C . \n";
  print shellScript "endif \n";

  print shellScript "if ( -e \"\$prototypeWDIR/flowPhiWgt.hist.root\" ) then \n";
  print shellScript "cp -Lr \$prototypeWDIR/flowPhiWgt.hist.root . \n";
  print shellScript "endif \n";

  print shellScript "if ( -e \"\$prototypeWDIR/zdcsmdConstants.root\" ) then \n";
  print shellScript "cp -Lr \$prototypeWDIR/zdcsmdConstants.root . \n";
  print shellScript "endif \n";

  print shellScript "if ( -e \"\$prototypeWDIR/PIDTable.root\" ) then \n";
  print shellScript "cp -Lr \$prototypeWDIR/PIDTable.root . \n";
  print shellScript "endif \n";

#  print shellScript "cp -Lr \$prototypeWDIR/.i386_linux24 . \n";
#  print shellScript "cp -Lr \$prototypeWDIR/.share . \n";
#  print shellScript "cp -Lr \$prototypeWDIR/.rh80_gcc32 . \n";
  print shellScript "cp -Lr \$prototypeWDIR/.sl302_gcc323 . \n";

  print shellScript " \@ count = 0 \n";
  print shellScript "foreach file (`cat \$prototypeWDIR/\$FILELIST`) \n";
  print shellScript "ln -s \$file  link.\$count.MuDst.root \n";
  print shellScript " \@ count ++ \n";
  print shellScript "end \n";
  print shellScript  "source \$GROUP_DIR/.starver $libVersion \n";
  print shellScript "root4star -b -q 'doFlowEvents.C(1000000,\"./\",\"\*MuDst.root\")' >& \${logfile} \n";
#  print shellScript "if ( -e \"$weightFile\" ) then \n";
#  print shellScript "mv -f $weightFile flowPhiWgt.hist.root\n";
#  print shellScript "mv -f run.log \$prototypeWDIR/sched\$JOBID.run.log.old\n";
#  print shellScript "root4star -b -q 'doFlowEvents.C(1000000,\"./\",\"\*MuDst.root\")' >& \${logfile} \n";
#  print shellScript "endif \n";
  print shellScript "if ( -e \"$yieldFile\" ) then \n";
  print shellScript "mv -f $yieldFile  \$prototypeWDIR/sched\$JOBID.$yieldFile \n";
  print shellScript "mv -f $cumulFile  \$prototypeWDIR/sched\$JOBID.$cumulFile \n";
  print shellScript "gzip $logFile \n";
  print shellScript "mv -f $logFile.gz    \$prototypeWDIR/sched\$JOBID.$logFile.gz \n";
  print shellScript "mv -f $meanGFile  \$prototypeWDIR/sched\$JOBID.$meanGFile \n";
  print shellScript "mv -f $weightFile \$prototypeWDIR/sched\$JOBID.$weightFile \n";
  print shellScript "else \n";
  print shellScript "mv -f $logFile    \$prototypeWDIR/sched\$JOBID.$logFile \n";
  print shellScript "echo \"Action did not produce the expected $cumulFile file\" \n";
  print shellScript "endif \n";
  print shellScript "rm -fr \$WDIR \n";
  print shellScript "exit 0 \n";
 close shellScript;


 }



  foreach $cent(@flowCents) { 


if ($ENV{HOST} =~ /pdsf/){ # not tested !
#if (0){
   open (macroFile,">$jobDir/$jobName/$cent/$schedName");

     print macroFile "<?xml version=\"1.0\" encoding=\"utf-8\" ?> \n";
     print macroFile "<job simulateSubmission =\"false\" maxFilesPerProcess =\"$maxFilesPerJob\"> \n";
     print macroFile "<command>$jobDir/$jobName/$cent/$runJob</command> \n";
     print macroFile "<stdout URL=\"file:$jobDir/$jobName/$cent/\$JOBID.out\" /> \n";
     print macroFile "<input URL=\"catalog:pdsf.nersc.gov?production=P04id||P04ie,filetype=daq_reco_mudst,trgsetupname=production62GeV,storage!=HPSS,tpc=1,ftpc=1,sanity=1,filename!~zerobias\" singleCopy=\"true\" nFiles=\"all\" /> \n";

     print macroFile "</job> \n";

   close macroFile;

   close prototypeMacro;

 }

elsif ($ENV{HOST} =~ /rcas/){ #RCF use file catalog
#elsif (1){ #RCF use file catalog
   open (macroFile,">$jobDir/$jobName/$cent/$schedName");

     print macroFile "<?xml version=\"1.0\" encoding=\"utf-8\" ?> \n";
     print macroFile "<job simulateSubmission =\"false\" maxFilesPerProcess =\"$maxFilesPerJob\"> \n";
     print macroFile "<command>$jobDir/$jobName/$cent/$runJob</command> \n";
     print macroFile "<stdout URL=\"file:$jobDir/$jobName/$cent/\$JOBID.out\" /> \n";
     print macroFile "<input URL=\"catalog:star.bnl.gov?production=P04id||P04ie,filetype=daq_reco_mudst,trgsetupname=production62GeV,storage!=HPSS,tpc=1,ftpc=1,sanity=1,filename!~zerobias\" singleCopy=\"true\" nFiles=\"all\" /> \n";

     print macroFile "</job> \n";

   close macroFile;
   close prototypeMacro;
 }



 }

 my $SubmitJobName="submitSched.csh";

  foreach $cent(@flowCents) {

    open ( submitScript, ">$jobDir/$jobName/$cent/$SubmitJobName");
     print submitScript "#! /bin/csh \n";
     print submitScript "cd $jobDir/$jobName/$cent \n";
if($ENV{HOST} =~ /pdsf/){ # not tested !
#if (0){
     print submitScript "module load java \n";
     print submitScript "star-submit $schedName \n";
 }
elsif($ENV{HOST} =~ /rcas/){ 
#elsif (1){ 
     print submitScript "star-submit $schedName \n";
 }
     print submitScript "exit 0 \n";
    close submitScript;
 }



  foreach $cent(@flowCents) { 
 print `chmod +x $jobDir/$jobName/$cent/$schedName \n`;
 print `chmod +x $jobDir/$jobName/$cent/$runJob \n`;
 print `chmod +x $jobDir/$jobName/$cent/$SubmitJobName \n`;

 print `csh -x $jobDir/$jobName/$cent/$SubmitJobName \n`;
 }

exit;




