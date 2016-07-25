#!/usr/bin/env perl 
use File::Basename;
use Sys::Hostname;
use Cwd;
my $pwd = Cwd::cwd(); print "pwd = $pwd\n";
my $bname = File::Basename::basename($pwd);
my $file = File::Basename::basename($pwd) . ".root";
my $dirname = "../../Histograms";
my $newfile = $dirname . "/" . $file;
my $checkjobs = "bjobs -l -q star_cas_prod | grep $bname";
#my $checkjobs = "bjobs -q star_cas_short";
my $cmd;
if (! -r $newfile) {
  my $temp_file = File::Basename::basename($pwd) . "_1.root";
  if (! -r $temp_file) {# jobs with StEvent are still running 
    #    while (my $log = `busers all | awk '{if ( \$5+\$6 > 0 ) print}' | grep fisyak`) {
    #    my $log = `$checkjobs`;
    #    print $log;
    #    die;
    for (;;) {
      my @log = `$checkjobs`; print "$#log \n";#@log\n";
      if ($#log == -1) {last;}
      sleep 60;
    }
    print "Done! with StEvent files\n";
    $cmd = "hsuma.pl"; print "$cmd\n";
    my $flag = system($cmd); print "$flag\n";
    sleep 60;
    for (;;) {
      my @log = `$checkjobs`; print "$#log \n";#@log\n";
      if ($#log == -1) {last;}
      sleep 60;
    }
    print "Done! with Addition jobs\n";
  }
  $cmd = "cd $pwd; root.exe -q -b *.root Hadd.C"; print "cmd = $cmd\n"; 
#  $cmd ="hadd " . $newfile . " " . $pwd . "/*.root";
  $flag = system("$cmd"); print "$flag\n";
  rename $file, $newfile or die "Can't rename $file to  $newfile"; 
} 
$cmd = "~/.dev/fit.pl " . $newfile . " select"; print "cmd = $cmd\n"; 
$flag = system($cmd); 
exit 0; 

