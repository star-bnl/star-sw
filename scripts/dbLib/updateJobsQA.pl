#! /usr/local/bin/perl -w
#
#  $Id: updateJobsQA.pl
#
#  L.Didenko
#
#  script to update newJobsQA table in LibraryJobs DB with number of hits for PXL, IST, SSD & MTD
#  and ratio for tracks reconstructed with HFT and without HFT.
#  Average values are taken from JobStatus table for nightly DEV library test.
#  
# 
############################################################################

use Class::Struct;
use File::Basename;

use DBI;

my $debugOn=0;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="LibraryJobs";


my $JobStatusT = "JobStatus";
my $JobQAT = "newJobsQA";

my $min;
my $hour;
my $mday;
my $mon;
my $year;
my $wday;
my $yday;
my $isdst;
my $thisday;
my $thistime;

my @Nday = ("Sun","Mon","Tue","Wed","Thu","Fri","Sat");

my %dayHash = (
                 "Sun" => 1,
                 "Mon" => 2,
                 "Tue" => 3,
                 "Wed" => 4,
                 "Thu" => 5,
                 "Fri" => 6,
                 "Sat" => 7,
                 );

#
# Set name of week day
#
  ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    $thisday = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];

$mon =  $mon + 1;

if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };


my $nowdate = ($year+1900)."-".$mon."-".$mday ;
   $thistime = $nowdate." ".$hour . ":" . $min . ":" . $sec;
       print ("Update at : ",  $thistime, "\n");

 my $ii = 0;
 my $iday;
 my $testDay;
 my $beforeDay;
 my $ik = 0;
 my $np = 8;


 $iday = $dayHash{$thisday};
 $testDay = $Nday[$iday - 1];
 $beforeDay = $Nday[$iday - 2];

my @hftpath = ();
my @nohftpath = ();


 $hftpath[0] = "/star/rcf/test/dev/daq_sl302.ittf/".$testDay."/year_2014/AuAu200_production_low_2014";
 $hftpath[1] = "/star/rcf/test/dev/daq_sl302.ittf_opt/".$testDay."/year_2014/AuAu200_production_low_2014";

 $hftpath[2] = "/star/rcf/test/dev/daq_sl302.ittf/".$testDay."/year_2015/production_pp200long_2015";
 $hftpath[3] = "/star/rcf/test/dev/daq_sl302.ittf_opt/".$testDay."/year_2015/production_pp200long_2015";

 $hftpath[4] = "/star/rcf/test/dev/daq_sl302.ittf/".$beforeDay."/year_2014/AuAu200_production_low_2014";
 $hftpath[5] = "/star/rcf/test/dev/daq_sl302.ittf_opt/".$beforeDay."/year_2014/AuAu200_production_low_2014";

 $hftpath[6] = "/star/rcf/test/dev/daq_sl302.ittf/".$beforeDay."/year_2015/production_pp200long_2015";
 $hftpath[7] = "/star/rcf/test/dev/daq_sl302.ittf_opt/".$beforeDay."/year_2015/production_pp200long_2015";

for ($ik = 0; $ik< $np; $ik++ ) {

 $nohftpath[$ik] = $hftpath[$ik];
 $nohftpath[$ik] =~ s/low/low.nohft/g;
 $nohftpath[$ik] =~ s/long/long.nohft/g;

print  $hftpath[$ik], "   % " ,$nohftpath[$ik], "\n";

}


my @avgTracks = ();
my @avgTracksn15 = ();
my @avgPrim = ();
my @avgPrimn15 = ();
my @hfjobID = ();


my @avgTracks_nohft = ();
my @avgTracksn15_nohft = ();
my @avgPrim_nohft = ();
my @avgPrimn15_nohft = ();

my @avgRatio_Trck = ();
my @avgRatio_Trckn15 = ();
my @avgRatio_Prim = ();
my @avgRatio_Primn15 = ();


   &StDbLibConnect();

 for ($ii = 0; $ii < 4; $ii++ ){

 $sql="SELECT avg_no_tracks, avg_no_tracksnfit15, avg_no_primaryT, avg_no_primaryTnfit15, jobID FROM $JobStatusT WHERE path = '$hftpath[$ii]' and avail = 'Y' and createTime like '$nowdate%' and jobStatus = 'Done' " ;

#  print "$sql\n";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow_array) {

       $avgTracks[$ii]    = $fields[0];
       $avgTracksn15[$ii] = $fields[1];
       $avgPrim[$ii]      = $fields[2];
       $avgPrimn15[$ii]   = $fields[3];
       $hfjobID[$ii]      = $fields[4];


    }
#       print "Check filling   ", $hftpath[$ii],"  ", $avgTracksn15[$ii], "\n";
}

#############
 
 for ($ii = 0; $ii < 4; $ii++ ){

 $sql="SELECT avg_no_tracks, avg_no_tracksnfit15, avg_no_primaryT, avg_no_primaryTnfit15 FROM $JobStatusT WHERE path = '$nohftpath[$ii]' and avail = 'Y' and createTime like '$nowdate%' and jobStatus = 'Done' " ;

#  print "$sql\n";
 
    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow_array) {

       $avgTracks_nohft[$ii]    = $fields[0];
       $avgTracksn15_nohft[$ii] = $fields[1];
       $avgPrim_nohft[$ii]      = $fields[2];
       $avgPrimn15_nohft[$ii]   = $fields[3];

    }
#      print "Check filling  nohft ", $nohftpath[$ii],"  ", $avgTracksn15[$ii], "\n";

 }

###############

 for ($ii = 4; $ii < 8; $ii++ ){

 $sql="SELECT avg_no_tracks, avg_no_tracksnfit15, avg_no_primaryT, avg_no_primaryTnfit15, jobID FROM $JobStatusT WHERE path = '$hftpath[$ii]' and avail = 'Y' and jobStatus = 'Done' " ;

#  print "$sql\n";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow_array) {

       $avgTracks[$ii]    = $fields[0];
       $avgTracksn15[$ii] = $fields[1];
       $avgPrim[$ii]      = $fields[2];
       $avgPrimn15[$ii]   = $fields[3];
       $hfjobID[$ii]      = $fields[4];

    }
}

#############
 
 for ($ii = 4; $ii < 8; $ii++ ){

 $sql="SELECT avg_no_tracks, avg_no_tracksnfit15, avg_no_primaryT, avg_no_primaryTnfit15 FROM $JobStatusT WHERE path = '$nohftpath[$ii]' and avail = 'Y' and jobStatus = 'Done' " ;

#  print "$sql\n";
 
    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow_array) {

       $avgTracks_nohft[$ii]    = $fields[0];
       $avgTracksn15_nohft[$ii] = $fields[1];
       $avgPrim_nohft[$ii]      = $fields[2];
       $avgPrimn15_nohft[$ii]   = $fields[3];

    }

 }

###############


  for ($ii = 0; $ii < $np; $ii++ ){

      if( defined $avgTracks[$ii] and defined $avgTracks_nohft[$ii] ) {

      if( $avgTracks_nohft[$ii] >= 0.000001 and $avgTracks[$ii] >= 0.000001 ) {
          $avgRatio_Trck[$ii] = $avgTracks[$ii]/$avgTracks_nohft[$ii] ;
      }else{
	  $avgRatio_Trck[$ii] = 0;
      }
      }else{
     $avgRatio_Trck[$ii] = 0;
      }      

      print "Ratio for number of tracks  ",$hftpath[$ii],"   ",$avgRatio_Trck[$ii], "\n";

    
#########
      if( defined $avgTracksn15[$ii] and defined $avgTracksn15_nohft[$ii] ) {
      if( $avgTracksn15_nohft[$ii] >= 0.000001 and $avgTracksn15[$ii] >= 0.000001 ) {
          $avgRatio_Trckn15[$ii] = $avgTracksn15[$ii]/$avgTracksn15_nohft[$ii] ;
      }else{
	  $avgRatio_Trckn15[$ii] = 0;
      }

      }else{
          $avgRatio_Trckn15[$ii] = 0;
      } 

#      print "Ratio for number of tracks nfit >=15 ",$avgRatio_Trckn15[$ii], "\n";
########
      if( defined $avgPrim[$ii] and defined $avgPrim_nohft[$ii] ) {
      if( $avgPrim_nohft[$ii] >= 0.000001 and $avgPrim[$ii] >= 0.000001 ) {
          $avgRatio_Prim[$ii] = $avgPrim[$ii]/$avgPrim_nohft[$ii] ;
      }else{
	  $avgRatio_Prim[$ii] = 0;
      }
     }else{
         $avgRatio_Prim[$ii] = 0;
    }

#      print "Ratio for number of primary tracks  ",$avgRatio_Prim[$ii], "\n";

########
      if( defined $avgPrimn15[$ii] and defined $avgPrimn15_nohft[$ii] ) {
      if( $avgPrimn15_nohft[$ii] >= 0.000001 and $avgPrimn15[$ii] >= 0.000001 ) {
          $avgRatio_Primn15[$ii] = $avgPrimn15[$ii]/$avgPrimn15_nohft[$ii] ;
      }else{
	  $avgRatio_Primn15[$ii] = 0;
      }
     }else{
          $avgRatio_Primn15[$ii] = 0;
     }
#      print "Ratio for number of primary tracks nfit >=15  ",$avgRatio_Primn15[$ii], "\n";

     if( defined $avgRatio_Trck[$ii] and defined $avgRatio_Trckn15[$ii] and defined $avgRatio_Prim[$ii] and defined $avgRatio_Primn15[$ii] ) {

  $sql= "update $JobQAT set avg_ratio_tracks = '$avgRatio_Trck[$ii]', avg_ratio_tracksnfit15 = '$avgRatio_Trckn15[$ii]', avg_ratio_primaryT = '$avgRatio_Prim[$ii]', avg_ratio_primaryTnfit15 = '$avgRatio_Primn15[$ii]' where path = '$hftpath[$ii]' and jobID = '$hfjobID[$ii]' and avail = 'Y' and jobStatus = 'Done' ";

    
     $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

     }
   }

  &StDbLibDisconnect();

 exit;

######################
sub StDbLibConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbLibDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}


