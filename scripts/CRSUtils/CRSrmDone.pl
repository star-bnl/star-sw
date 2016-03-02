#! /usr/local/bin/perl -w
#
# L. Didenko
###############################################################################

 my @statlist = ();
 my @joblist = ();
 my $timestamp ;
 my $fullname;


 @statlist = `farmstat`;

my $Ndone = 0;
my $Nerror = 0;

my $jobname;
my @prt = ();
my @wrd = ();
my $year = ();

  ($sec,$min,$hour,$mday,$mon,$yr) = localtime;

  
    $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $min = '0'.$sec };

$year = $yr + 1900 ;

  $timestamp = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

 print $timestamp, "\n";

  foreach $line (@statlist) {
     chop $line ;
#   print  $line, "\n";
    @prt = ();
    @prt = split (" ", $line);

     if ($prt[0] eq "DONE") {
	 $Ndone =  $prt[1];
  }
 }
     if($Ndone >= 1 ) {
        `crs_job -kill_status DONE`;
     }

     print "Killed  ", $Ndone, "  jobs DONE", "\n";
   
   `crs_job -fix_jobs`;
    
exit;

