#! /opt/star/bin/perl -w
#
#  
#
#     read_mail.pl - script to read production emails and move
#          jobfiles if job crashed from archive to jobfiles to resubmit them  
#  L.Didenko
###############################################################################


my $dir_arch = "/star/u2e/starreco/mdc3/requests/tfs/archive";
my $dir_job = "/star/u2e/starreco/mdc3/requests/tfs/jobfiles";
my @maifile;
my $boxfile = "mbox";
my $mail_line;
my $num_line = 0;
my $next_line;
my $messg_count = 0;
my $staging_count = 0;
my $abort_count = 0;
my $crash_count = 0;
my $status_line;
my $job_line;
my $job_file;
my @parts;
my $num_jbfile = 0;
my @fileArr = ();
my $eachJob;
my $kl = 0;
 
open (MAILFILE, $boxfile ) or die "cannot open $boxfile: $!\n";

@mailfile = <MAILFILE>;

  foreach $mail_line (@mailfile) {
     chop $mail_line ;
#   print "Num line = ", $num_line, "Line = ", $mailfile[$num_line],  "\n";

   if ($mail_line =~ /Message-Id/) {
      $messg_count++;
   };
   if ($mail_line =~ /job_/) {
      $status_line = $mail_line;
     if ( $status_line =~ /staging failed/) {
       $staging_count++;
     $job_line = $mailfile[$num_line + 3];   
       @parts = split (":", $status_line);
     $job_file = $parts[1];        
      $fileArr[$num_jbfile] = $job_file; 
     $num_jbfile++ ;
     print "Job file name when staging failed: ", $job_file, "\n";
     }
     elsif ($status_line =~ /aborted/) {
       $abort_count++;
     $job_line = $mailfile[$num_line + 3];
      @parts = split (":", $job_line);
     $job_file = $parts[1];        
      $fileArr[$num_jbfile] = $job_file; 
     $num_jbfile++ ;      
     print "Job file name when aborted: ", $job_file, "\n";
     }
     
     elsif ($status_line =~ /killed/) {
      
       $abort_count++;
     $job_line = $mailfile[$num_line + 3];
      @parts = split (":", $job_line);
     $job_file = $parts[1];        
      $fileArr[$num_jbfile] = $job_file; 
     $num_jbfile++ ;      
     print "Job file name when killed: ", $job_file, "\n";
     }

     elsif ($status_line =~ /crashed/) {
      
       $crash_count++;
     $job_line = $mailfile[$num_line + 3];
      @parts = split (":", $job_line);
     $job_file = $parts[1];        
           $fileArr[$num_jbfile] = $job_file; 
     $num_jbfile++ ; 
     print "Job file name when crashed: ", $job_file, "\n";
     }

   }
      $num_line++;
   }

 
  print "Number of emails: ", $messg_count, "\n"; 
  print "Number of jobs with staging failed: ", $staging_count, "\n";
  print "Number of jobs aborted: ", $abort_count, "\n";
  print "Number of jobs crashed: ", $crash_count, "\n";

 chdir $dir_arch;
 chomp @fileArr; 

foreach $eachJob (@fileArr) {
               
        print "jobfile: ", $eachJob, "\n";
         `mv $eachJob $dir_job \n` ;
          
      }
exit;
