#! /opt/star/bin/perl -w
#
#  
#
#     read_mail.pl - script to read production emails and move
#          jobfiles if job crashed from archive to jobfiles to resubmit them  
#  L.Didenko
###############################################################################


my $mail_line;
my $status_line;
my $job_file = "none";
my $jbStat = "n/a";
my @parts;
my $nodeID = "n/a";
 
my @wrd;

$now = localtime;
($sec,$min,$hour,$mday,$mon) = localtime;
$thisday = $mon . "." .$mday;

$outfile = "mail" . "_" .$thisday . "_" . "out";

while (<>) {
  $mail_line = $_;

#    print "$mail_line \n";
 
   if ($mail_line =~ /job_/) {
      $status_line = $mail_line;
 
    if ( $status_line =~ /done/) {
       $jbStat = "done";
       @wrd = split (" ",$status_line);
       $nodeID = $wrd[3]; 
     }
     elsif ( $status_line =~ /staging failed/) {
       $jbStat = "staging failed";
       @wrd = split (" ",$status_line);
       $nodeID = $wrd[4]; 
     }

     elsif ($status_line =~ /aborted/) {
       $jbStat = "aborted";
       @wrd = split (" ",$status_line);
       $nodeID = $wrd[3]; 
     }
     
     elsif ($status_line =~ /killed/) {
      $jbStat = "killed";
       @wrd = split (" ",$status_line);
       $nodeID = $wrd[3]; 
     }
     elsif ($status_line =~ /file not found/) {
      $jbStat = "file not found";
       $nofiles_count++;
       $nodeID = "n/a"; 
     }

     elsif ($status_line =~ /crashed/) {
      $jbStat = "crashed";
       @wrd = split (" ",$status_line);
       $nodeID = $wrd[3]; 
     }

    } 
      if ($mail_line =~ /Job Description File/) {
       @parts = split (":", $status_line);
     $job_file = $parts[1];
    }
 }

 open (OUT,">> $outfile") or die "Can't open $outfile";
  print "JobInfo: ", $jbStat,"  %  ",$nodeID,"  %  ",$job_file,  "\n"; 

  close (OUT);

exit;
