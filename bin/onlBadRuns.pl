#!/usr/bin/env perl
# http://drupal.star.bnl.gov/STAR/comp/db/onlinedb/online-sever-port-map/
# Run/Year	       NODE	Port
my @Runs = (
    'Run1'        => {node => 'dbbak.starp.bnl.gov', port => '3400'},
    'Run2' 	  => {node => 'dbbak.starp.bnl.gov', port => '3401'},
    'Run3' 	  => {node => 'dbbak.starp.bnl.gov', port => '3402'},
    'Run4' 	  => {node => 'dbbak.starp.bnl.gov', port => '3403'},
    'Run5' 	  => {node => 'dbbak.starp.bnl.gov', port => '3404'},
    'Run6' 	  => {node => 'dbbak.starp.bnl.gov', port => '3405'},
    'Run7' 	  => {node => 'dbbak.starp.bnl.gov', port => '3406'},
    'Run8' 	  => {node => 'dbbak.starp.bnl.gov', port => '3407'},
    'Run9' 	  => {node => 'dbbak.starp.bnl.gov', port => '3408'},
    'Run10' 	  => {node => 'dbbak.starp.bnl.gov', port => '3409'},
    'Run11' 	  => {node => 'dbbak.starp.bnl.gov', port => '3410'},
    'Run12' 	  => {node => 'dbbak.starp.bnl.gov', port => '3411'},
    'Run13' 	  => {node => 'dbbak.starp.bnl.gov', port => '3412'},
    'Run14' 	  => {node => 'dbbak.starp.bnl.gov', port => '3413'},
    'Run15' 	  => {node => 'dbbak.starp.bnl.gov', port => '3414'},
    'Run16' 	  => {node => 'dbbak.starp.bnl.gov', port => '3415'},
    'Run17' 	  => {node => 'dbbak.starp.bnl.gov', port => '3416'},
    'Run18' 	  => {node => 'dbbak.starp.bnl.gov', port => '3417'},
    'Run19' 	  => {node => 'dbbak.starp.bnl.gov', port => '3418'},
    'CURRENT'     => {node => 'onldb.starp.bnl.gov', port => '3501'}
	   );
my $Run = "CURRENT";
my $select = "";
if ($#ARGV >= 0) {
  $Run = $ARGV[0];
}
#  $select = " -e \'select runNumber,glbSetupName, beginTime from runDescriptor order by beginTime\'";
  $select = " -e \'SELECT t1.runNumber,t1.glbSetupName, t1.beginTime, t2.scaleFactor, t3.rtsStatus, t3.shiftLeaderStatus  FROM runDescriptor AS t1 INNER JOIN  magField AS t2  INNER JOIN  runStatus  AS t3 ON t1.runNumber = t2.runNumber and  t1.runNumber = t3.runNumber WHERE  (t3.rtsStatus != 0 OR t3.shiftLeaderStatus != 0) AND t1.runNumber > 20361000 order by t1.beginTime\'";
#  $select = " -e \'SELECT t1.runNumber,t1.glbSetupName, t1.beginTime, t2.scaleFactor, t3.rtsStatus, t3.shiftLeaderStatus  FROM runDescriptor AS t1 INNER JOIN  magField AS t2  INNER JOIN  runStatus  AS t3 ON t1.runNumber = t2.runNumber and  t1.runNumber = t3.runNumber where  t3.rtsStatus = 0 AND t3.shiftLeaderStatus = 0 order by t1.beginTime\'";
my $def = {@Runs};
#print "$Run => node: $def->{$Run}->{node} port: $def->{$Run}->{port}\n";
#my $cmd = "mysql Conditions_rich -h $def->{$Run}->{node} -P $def->{$Run}->{port}";
my $cmd = "mysql RunLog -h $def->{$Run}->{node} -P $def->{$Run}->{port}";
$cmd .= $select;
#print "cmd = $cmd\n";
my $flag      = system($cmd);
exit 0

# foreach r ( `onlBadRuns.pl | awk '{print $1}'` ) 
#   ls -1d ???/${r}; if ($?) continue; 
#   echo "mv $r to Junk";  
#   mv ???/${r} Junk;
# end 
