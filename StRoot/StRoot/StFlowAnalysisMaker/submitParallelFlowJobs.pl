#! /opt/star/bin/perl -w


$help =<<"EOF";
usage: $0 --f=
--f, the configuration file of a job.
EOF

use Cwd 'abs_path';     # aka realpath()
use Getopt::Long;

GetOptions( "f=s" => \$configFile   );

 die $help," Pl. specify the configuration file."
 unless defined $configFile;

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
 "cen1",  # do not change these names
 "cen2",
 "cen3",
 "cen4",
 "cen5",
 "cen6",
 "cen7",
 "cen8"
);




my $macroName   = $ConfigMap{"macroName"};
my $libVersion  = $ConfigMap{"libVersion"};
my $runJob      = $ConfigMap{"scriptName"};
my $jobPrefix   = $ConfigMap{"partnPrefix"};
my $picoDstLink = $ConfigMap{"picoDSTLink"};
my $NPartition  = $ConfigMap{"NPartition"};
my $jobDir      = $ConfigMap{"jobDir"};
my $jobName     = $ConfigMap{"jobName"};
my $dataDir     = $ConfigMap{"dataDir"};
my $logFile     = $ConfigMap{"logFileName"};

# get a list of all flow pico dsts
 @totalList=glob($dataDir."/*.root");

# get number of files for one partition.
 $NFilesInOnePartition = int(scalar(@totalList)/$NPartition); 



# make job directories
 print `mkdir $jobDir/$jobName`;

# copying source code to $jobName
print `cp -r $jobDir/StRoot $jobDir/$jobName`;

print `cp $configFile $jobDir/$jobName/$configFile`;



# cons
 my $consFileName="consDir";
 open ( consScript, ">".$consFileName.".csh");
 print consScript  "#!/bin/csh \n";
 print consScript  "cd  $jobDir/$jobName \n";
 print consScript  "source /auto/u/aihong/.starver $libVersion \n";
 print consScript  "cons \n";
 close consScript;

 print "$consFileName.csh has been generated \n";
 print "now run $consFileName.csh  : \n";
 print `csh -x $jobDir/$consFileName.csh`;
 print `rm $jobDir/$consFileName.csh`;

# making directories for partitioned jobs
 for ($m=1; $m<$NPartition+1; $m++) { 
 print `mkdir $jobDir/$jobName/$jobPrefix$m`; 
 print `mkdir $jobDir/$jobName/$jobPrefix$m/$picoDstLink`; # dir for picoDST links for a partition
 
 my @END;

 if ($m <$NPartition) { 
@END=splice(@totalList, -$NFilesInOnePartition)  #pop up NFilesInOnePartition files
 } else {@END=@totalList;}   # the last partition.

# making links to pico dst
 foreach $eachFile (@END) { 
 ($stripped = $eachFile) =~ s/$dataDir//; #now $dataDir begin with "/"
 print `ln -s $eachFile $jobDir/$jobName/$jobPrefix$m/$picoDstLink$stripped \n`;
 }




# making centrality directories, and lib links
 foreach $cent(@flowCents) {
 print `mkdir $jobDir/$jobName/$jobPrefix$m/$cent`;
 print `ln -s $jobDir/$jobName/.share  $jobDir/$jobName/$jobPrefix$m/$cent/.share   \n`;
 print `ln -s $jobDir/$jobName/.i386_redhat61  $jobDir/$jobName/$jobPrefix$m/$cent/.i386_redhat61   \n`;

# producing macro
   open (macroFile,">$jobDir/$jobName/$jobPrefix$m/$cent/$macroName");
   open (prototypeMacro,$jobDir."/doFlowEvents.C");

   while ($eachLine = <prototypeMacro>) {

     if ($eachLine=~/SetCent/) {
    if ($cent=~/cen1/){$eachLine="    StFlowCutEvent::SetCent(1, 1); \n";}
    if ($cent=~/cen2/){$eachLine="    StFlowCutEvent::SetCent(2, 2); \n";}
    if ($cent=~/cen3/){$eachLine="    StFlowCutEvent::SetCent(3, 3); \n";}
    if ($cent=~/cen4/){$eachLine="    StFlowCutEvent::SetCent(4, 4); \n";}
    if ($cent=~/cen5/){$eachLine="    StFlowCutEvent::SetCent(5, 5); \n";}
    if ($cent=~/cen6/){$eachLine="    StFlowCutEvent::SetCent(6, 6); \n";}
    if ($cent=~/cen7/){$eachLine="    StFlowCutEvent::SetCent(7, 7); \n";}
    if ($cent=~/cen8/){$eachLine="    StFlowCutEvent::SetCent(8, 8); \n";}
     }

     print macroFile $eachLine;
 }
  
   close macroFile;
   close prototypeMacro;


# producing shell script

 open (shellScript,">$jobDir/$jobName/$jobPrefix$m/$cent/$runJob");
  print shellScript "#! /bin/csh \n";
  print shellScript "#BSUB -J $jobName/$jobPrefix$m/$cent \n";
  print shellScript "#BSUB -u aihong\@cnr2.kent.edu\n"; 
  print shellScript "set logfile=$logFile \n";
  print shellScript "set rundir=$jobDir/$jobName/$jobPrefix$m/$cent \n";
  print shellScript "\n";
  print shellScript "cd \$rundir \n";
  print shellScript  "source /auto/u/aihong/.starver $libVersion \n";
  print shellScript "root4star -b << EOF >>\& \$logfile \n";
  print shellScript ".which   doFlowEvents.C \n";
  print shellScript ".x doFlowEvents.C(1000000,\"$jobDir/$jobName/$jobPrefix$m/$picoDstLink\",\"*flowpicoevent.root\") \n";
  print shellScript ".q \n";
  print shellScript "EOF \n";
  print shellScript "exit 0 \n";
 close shellScript;


 }

}



 for ($m=1; $m<$NPartition+1; $m++) { 

  foreach $cent(@flowCents) { 

# print `bsub -q medium -R "linux select[defined(dv36io)] rusage[dv36io=50]" <  $jobDir/$jobName/$jobPrefix$m/$cent/$runJob`;

  print `bsub -q medium  <  $jobDir/$jobName/$jobPrefix$m/$cent/$runJob`;
#  print "$jobDir/$jobName/$jobPrefix$m/$cent/$runJob \n";

 }
}
exit;




#perl parallFlowJobSubmit.pl --d /auto/pdsfdv36/starebye/aihong/MinbiasP01hi/copiedFromPdsfdv08 --i 5 --o test


####$pwd = abs_path ( $ENV { 'PWD' } );
