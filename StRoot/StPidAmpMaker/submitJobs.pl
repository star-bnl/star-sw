#! /opt/star/bin/perl -w

$NCentBins=3;
$NDcaBins=2;
$NChgBins=2;


my $currentDir=`pwd`;

# get rid of the \n charater in $currentDir
chop($currentDir);


for ($i=0; $i<$NCentBins; $i++){
  for ($j=0; $j<$NDcaBins; $j++){
     for ($k=0; $k<$NChgBins; $k++) {

# print "$i";

 my $runJob="run$i$j$k";
 my $newAnalysis="PidFit$i$j$k";

 my $jobfile;

 open (jobfile,">$currentDir/$runJob.csh");

 print jobfile "#! /star/u/aihong/bin/tcsh -f \n";

 print jobfile "#BSUB -J $newAnalysis \n";
 print jobfile "#BSUB -u aihong\@cnr2.kent.edu\n"; 

 print jobfile "set logfile=$runJob.log \n";
 print jobfile "set rundir=$currentDir \n";

 print jobfile "\n";

 print jobfile "cd \$rundir \n";
 print jobfile "source /star/u/aihong/.starver SL01l \n";
# print jobfile "root4star -b << EOF >>\& \$logfile \n";
 print jobfile "root4star -b << EOF >>\& /dev/null \n";
 print jobfile ".which  fitHisto.C \n";
 print jobfile ".L fitHisto.C\n";
 print jobfile "fit($i,$j,$k) \n";
 print jobfile ".q \n";
 print jobfile "EOF \n";

 close jobfile;
      }
   }
}



for ($i=0; $i<$NCentBins; $i++){
  for ($j=0; $j<$NDcaBins; $j++){
     for ($k=0; $k<$NChgBins; $k++) {
 my $runJob="run$i$j$k";
 print `bsub -q star_cas <  $currentDir/$runJob.csh`;
  
     }
   }
}

exit;
