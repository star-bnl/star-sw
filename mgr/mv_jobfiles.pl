#! /opt/star/bin/perl -w
#
#  
#
#     mv_jobfiles.pl - script to move jobfiles for crashed jobs
#          from archive to jobfiles directory 
#  L.Didenko
###############################################################################

my $dir_arch = "/star/u/starreco/P01he/requests/daq/archive";
my $dir_job = "/star/u/starreco/P01he/requests/daq/jobfiles";

my @maifile;
my $mail_line;
my $status_line;
my $job_file = "none";
my $jbStat = "n/a";
my @parts;
my @wrd;
my @jobFList;
my $numJbfile = 0;
my $outname;
my $outfile;
my $crash_count = 0;
my $stag_count = 0;
my $abort_count = 0;
my $kill_count = 0;
my $fnotf_count = 0;

$now = localtime;
($sec,$min,$hour,$mday,$mon) = localtime;

foreach my $int ( $mon,$mday ){
  $int < 10 and $int = '0'.$int;
   $thisday .= $int;
}

$outname = "mail" . "_" .$thisday . "_" . "out";
$outfile = "/star/u/starreco/" . $outname;
#print $outfile, "\n";

open (MAILFILE, $outfile ) or die "cannot open $outfile: $!\n";


 @mailfile = <MAILFILE>;

  foreach $mail_line (@mailfile) {
     chop $mail_line ;
     if ($mail_line =~ /JobInfo/ ) {
      @wrd = split ("%", $mail_line);
      $jbStat = $wrd[1];
      $job_file = $wrd[3]; 

      if ($jbStat =~ /crashed/) {
       $crash_count++;
     $jobFList[$numJbfile] = $job_file;
     $numJbfile++;
     }
      elsif ($jbStat =~ /aborted/) {
      $abort_count++; 
     $jobFList[$numJbfile] = $job_file;
     $numJbfile++;  
     }
     elsif ($jbStat =~ /staging failed/) {
      $stag_count++; 
     $jobFList[$numJbfile] = $job_file;
     $numJbfile++;
     }
     elsif ($jbStat =~ /killed/) {
      $kill_count++;     
     $jobFList[$numJbfile] = $job_file;
     $numJbfile++;
  } 
     elsif ($jbStat =~ /file not found/) {
      $fnotf_count++;     
     $jobFList[$numJbfile] = $job_file;
     $numJbfile++;     
     }
    }
  }

chdir $dir_arch;
 
for ($ll = 0; $ll < scalar(@jobFList); $ll++) {
      $myfile = $jobFList[$ll];
     
  print "jobfile: ", $myfile, "\n";  
    `mv $myfile $dir_job \n`;       

}
  print "Number of jobs with staging failed: ", $stag_count, "\n";
  print "Number of jobs aborted: ", $abort_count, "\n";
  print "Number of jobs crashed: ", $crash_count, "\n";
  print "Number of jobs killed:  ", $kill_count, "\n";
  print "Number of files not found: ", $fnotf_count, "\n";

exit;

