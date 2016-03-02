#!/usr/local/bin/perl -w
#
#  $Id: killCRSjobs.pl
#
#  L. Didenko
#
#  $Log: script to kill CRS jobs in state RUNNING by prodtag and trigger set name 
#
###############################################################################

use File::Basename;

 my $trgtag = $ARGV[0];
 my $prdtag = $ARGV[1];


 my @jbstate  = ();
 my @prodtags = ();
 my @jbtrigs = ();
 my @prt = ();
 my @jbId = ();
 my @runId = ();
 my $njob = 0;
 my @outfile = ();
 my @wrd = ();
 my @joblist  = ();

 my @jbIDset = ();
 
 my $nkjb = 0;
 
########### jobs in RUNNING

 @joblist = ();

 @joblist = `crs_job -stat | grep RUNNING` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "RUNNING";  

     $njob++;
 } 


for($ii = 0; $ii<$njob; $ii++) {

    @outfile = ();
    @outfile = `crs_job -long $jbId[$ii] | grep MuDst`;

     foreach my $fline (@outfile) {
     if ( $fline =~ /starreco/ ) {
       @prt = ();
       @prt = split("starreco", $fline) ;
       @wrd = ();
       @wrd = split("/", $prt[1]) ;
       $jbtrigs[$ii] = $wrd[2];
       $prodtags[$ii] = $wrd[4];
       $runId[$ii] = $wrd[7];

       if($jbtrigs[$ii] eq  $trgtag and $prodtags[$ii] eq $prdtag ) {
 
	   $jbIDset[$nkjb] = $jbId[$ii];
           $nkjb++
       }
      }
     }
  }

       if($nkjb >1) {

    for($ik = 0; $ik<$nkjb; $ik++) {
      
     'crs_job -kill -f $jbIDset[$ik]' ;


       print "Killed job with JobID:  ", ,$jbIDset[$ik], "\n";
       

    `crs_job -destroy -f -s KILLED` ;

  }

 }else{
    print "No jobs for  ", $trgtag,"  dataset and   ",$prdtag, "   production tag",  "\n";

 }

exit;


