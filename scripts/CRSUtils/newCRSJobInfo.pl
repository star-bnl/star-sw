#!/usr/local/bin/perl -w
#
#  $Id: newCRSJobInfo.pl
#
#  L. Didenko
#
#  $Log: script to fillin table CRSJobsInfo with jobfiles properties and status 
#
###############################################################################

use DBI;
use File::Basename;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

# Tables
$crsJobStatusT = "CRSJobsInfo";


my %monthHash = (
                  Jan => "1",
                  Feb => "2",
                  Mar => "3",
                  Apr => "4",
                  May => "5",
                  Jun => "6",
                  Jul => "7",
                  Aug => "8",
                  Sep => "9",
                  Oct => "10",
                  Nov => "11",
                  Dec => "12",
                  );


 my $mmon;
 my $nmonth;
 my $jstime;

 my @jbstate  = ();
 my @prodtags = ();
 my @jbtrigs = ();
 my @jbfiles = ();
 my @jbstreams = ();
 my @prt = ();
 my @jbId = ();
 my @runId = ();
 my $njob = 0;
 my @outfile = ();
 my @wrd = ();
 my @spl = ();
 my @joblist  = ();
 my @errlines = ();
 my @jberror = ();
 my @jbnode = ();
 my @jobnode = ();
 my @strflag = ();
 my $Tperror = 0;
 my @jobstart = ();
 my @jbstart = ();
 my @jobtri = ();
 my @jbtri = ();
 my $jdate = "0000-00-00";
 my $jtime = "0000-00-00 00:00:00";


 my $year;
 my $mon = 0;
 my $mday = 0;
 my $hour = 0;
 my $min = 0;
 my $sec = 0;
 my $thisdate ;

 my @prevdate = ();
 my $nn = 0;
 my @runstart = ();
 my $nk = 0;
 my $kk = 0;

 ($sec,$min,$hour,$mday,$mon,$yr) = localtime;


    $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };

 $year = $yr + 1900;

 $thisdate = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;


 print "-------------------------------------","\n";
 print $thisdate, "\n";

   &StcrsdbConnect();

 $sql="SELECT DISTINCT flag  FROM $crsJobStatusT where flag <> ' ' ";

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

       while( my $ftmp = $cursor->fetchrow() ) {

          $strflag[$kk] = $ftmp;
          $kk++;
       }
    $cursor->finish();

  for ($ii = 0; $ii< $kk; $ii++) {
  
  if ($strflag[$ii] eq "Start" ) {

      print "There is a flag ", $strflag[$ii], "\n";

      print "Script JobInfo is running","\n";

   exit;

  }  
}

  $sql="SELECT DISTINCT runDate  FROM $crsJobStatusT where flag = 'Start' ";

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

       while( $ftmp = $cursor->fetchrow() ) {
          $runstart[$nk] = $ftmp;
          $nk++;
       }
    $cursor->finish();



  $sql="SELECT DISTINCT runDate  FROM $crsJobStatusT where flag = 'Done' ";

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

       while( $tmp = $cursor->fetchrow() ) {
          $prevdate[$nn] = $tmp;
          $nn++;
       }
    $cursor->finish();

  print "Previous datetime ", "\n";
  for ($ii=0;$ii<$nn;$ii++) {
   print  $prevdate[$ii], "\n";
 }


  $sql= "insert into $crsJobStatusT set runDate = '$thisdate', flag = 'Start' ";

      $dbh->do($sql) || die $dbh->errstr; 



########### jobs DONE

 @joblist = ();

 @joblist = `crs_job -stat | grep DONE` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "DONE";  
     $jberror[$njob] = 0;
     $jbnode[$njob] = "none";
     $jbstart[$njob] ="0000-00-00 00:00:00";
     $jbtri[$njob] = 0; 
     $njob++;
 }


########### jobs in QUEUED

 @joblist = ();

 @joblist = `crs_job -stat | grep QUEUED` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "QUEUED";  
     $jberror[$njob] = 0;
     $jbnode[$njob] = "none";   
     $jbtri[$njob] = 0; 

####    find start time

    @jobstart = ();
    @jobstart = `crs_job -long $jbId[$njob] | grep 'Status: QUEUED'` ;

   foreach my $jstart (@jobstart) {
     chop $jstart ;
#     print $jstart, "\n";

       @prt = ();
       @prt = split(/\(/, $jstart) ;
       $jdate =  $prt[1];
###
       @prt = ();
       @prt = split(/\)/, $jdate) ;
       $jstime =  $prt[0];
###
       @prt = ();
       @prt = split(" ", $jstime) ;
       $mmon = $prt[0];
       $nmonth = $monthHash{$mmon};

     if( $nmonth < 10) { $nmonth = '0'.$nmonth };

       $jtime = $prt[2]."-".$nmonth."-".$prt[1]." ".$prt[3];
       $jbstart[$njob] = $jtime;

  }
################
     $njob++;
 }

########### jobs in STAGING

 @joblist = ();

 @joblist = `crs_job -stat | grep STAGING` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "STAGING";  
     $jberror[$njob] = 0;
     $jbnode[$njob] = "none";   

####  find start time

    @jobstart = ();
    @jobstart = `crs_job -long $jbId[$njob] | grep 'Status: STAGING'` ;

  foreach my $jstart (@jobstart) {
     chop $jstart ;
#     print $jstart, "\n";
     if ( $jstart =~ /Created:/ ) {

       @prt = ();
       @prt = split(/\(/, $jstart) ;
       $jdate =  $prt[1];

       @prt = ();
       @prt = split(/\)/, $jdate) ;
       $jstime =  $prt[0];

       @prt = ();
       @prt = split(" ", $jstime) ;
       $mmon = $prt[0];
       $nmonth = $monthHash{$mmon};

     if( $nmonth < 10) { $nmonth = '0'.$nmonth };

       $jtime = $prt[2]."-".$nmonth."-".$prt[1]." ".$prt[3];
       $jbstart[$njob] = $jtime;

    }
  }

     @jobtri = ();
     @jobtri = `crs_job -long $jbId[$njob] | grep Tries: ` ;

 foreach my $jtline (@jobtri) {
      chop $jtline ;
#     print $jtline, "\n";
      @prt = ();

      @prt = split(" ", $jtline) ;
      $jbtri[$njob] =  $prt[1];

#     print  $jbId[$njob], "  ", $jbstart[$njob], "  ", $jbtri[$njob], "\n";

 }

###########
     $njob++;
 } 

########### jobs in SUBMITTED

 @joblist = ();

 @joblist = `crs_job -stat | grep SUBMITTED` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "SUBMITTED";  
     $jberror[$njob] = 0;
     $jbnode[$njob] = "none";     

####    find start time

    @jobstart = ();
    @jobstart = `crs_job -long $jbId[$njob] | grep 'Status: SUBMITTED'` ;

     foreach my $jstart (@jobstart) {
     chop $jstart ;
#     print $jstart, "\n";

     if ( $jstart =~ /Created:/ ) {

       @prt = ();
       @prt = split(/\(/, $jstart) ;
       $jdate =  $prt[1];
###
       @prt = ();
       @prt = split(/\)/, $jdate) ;
       $jstime =  $prt[0];
###
       @prt = ();
       @prt = split(" ", $jstime) ;
       $mmon = $prt[0];
       $nmonth = $monthHash{$mmon};

     if( $nmonth < 10) { $nmonth = '0'.$nmonth };

       $jtime = $prt[2]."-".$nmonth."-".$prt[1]." ".$prt[3];
       $jbstart[$njob] = $jtime;
   }
  }

     @jobtri = ();
     @jobtri = `crs_job -long $jbId[$njob] | grep Tries: ` ;

 foreach my $jtline (@jobtri) {
      chop $jtline ;
#      print $jtline, "\n";
      @prt = ();

      @prt = split(" ", $jtline) ;
      $jbtri[$njob] =  $prt[1];

#     print  $jbId[$njob], "  ", $jbstart[$njob], "  ", $jbtri[$njob], "\n";

 }

######
     $njob++;
 } 


########### jobs in IMPORTING

 @joblist = ();

 @joblist = `crs_job -stat | grep IMPORTING` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "IMPORTING";  
     $jberror[$njob] = 0;
#     $jbnode[$njob] = "none";
     $jbstart[$njob] ="0000-00-00 00:00:00";
     $jbtri[$njob] = 0; 

    @jobnode = ();
    @jobnode = `crs_job -print $jbId[$njob] | grep Machine`;

   foreach my $jnode (@jobnode) {
     chop $jnode ;
#     print $jnode, "\n";
    if ( $jnode =~ /Machine/ ) {
       @prt = ();
       @prt = split(" ", $jnode) ;
     $jbnode[$njob] = $prt[1];
    }
  }

####
     $njob++;
 } 

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
     $jberror[$njob] = 0;
#     $jbnode[$njob] = "none";  
     $jbtri[$njob] = 0; 

    @jobnode = ();
    @jobnode = `crs_job -print $jbId[$njob] | grep Machine`;

   foreach my $jnode (@jobnode) {
     chop $jnode ;
#     print $jnode, "\n";
    if ( $jnode =~ /Machine/ ) {
       @prt = ();
       @prt = split(" ", $jnode) ;
     $jbnode[$njob] = $prt[1];
    }
  }

####### find start time

    @jobstart = ();
#    @jobstart = `crs_job -long $jbId[$njob] | grep Start`;

    @jobstart = `crs_job -print $jbId[$njob] | grep 'Status: RUNNING'`;

   
   foreach my $jstart (@jobstart) {
     chop $jstart ;
####     print $jstart, "\n";

       @prt = ();
       @prt = split(/\(/, $jstart) ;
       $jdate =  $prt[1];

#####
       @prt = ();
       @prt = split(/\)/, $jdate) ;      
       $jstime =  $prt[0];

############   to come back remove next part

       @prt = ();
       @prt = split(" ", $jstime) ;
       $mmon = $prt[0];
       $nmonth = $monthHash{$mmon};

     if( $nmonth < 10) { $nmonth = '0'.$nmonth };

       $jtime = $prt[2]."-".$nmonth."-".$prt[1]." ".$prt[3];
       $jbstart[$njob] = $jtime;
 
 }

################
     $njob++;
 } 


########### jobs in EXPORTING

 @joblist = ();

 @joblist = `crs_job -stat | grep EXPORTING` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "EXPORTING";  
     $jberror[$njob] = 0;
     $jbtri[$njob] = 0;
     $jbstart[$njob] ="0000-00-00 00:00:00"; 

    @jobnode = ();
    @jobnode = `crs_job -print $jbId[$njob] | grep Machine`;

   foreach my $jnode (@jobnode) {
     chop $jnode ;
#     print $jnode, "\n";
    if ( $jnode =~ /Machine/ ) {
       @prt = ();
       @prt = split(" ", $jnode) ;
     $jbnode[$njob] = $prt[1];
    }
  }
#######
     $njob++;
 } 


########### jobs in ERROR

 @joblist = ();

 @joblist = `crs_job -stat | grep ERROR` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "ERROR";  
     $jbtri[$njob] = 0; 
     $jbstart[$njob] ="0000-00-00 00:00:00";

     @errlines = ();
     @errlines = `crs_job -long $jbId[$njob] | grep Error`; 

   foreach my $erline (@errlines) {
     chop $erline ;
#   print $erline, "\n";
     if ( $erline =~ /Error/ ) {

     @prt = ();
     @prt = split (" ", $erline);

#  print "Error line : ", $pt[1],"  ", $pt[2],"  ",$pt[3], "\n";

     $Tperror = $prt[2];
     $Tperror =~ s/://g;

      }
    }
     $jberror[$njob] =  $Tperror;

    @jobnode = ();
    @jobnode = `crs_job -long $jbId[$njob] | grep Machine`;

   foreach my $jnode (@jobnode) {
     chop $jnode ;
#     print $jnode, "\n";
    if ( $jnode =~ /Machine/ ) {
       @prt = ();
       @prt = split(" ", $jnode) ;
     $jbnode[$njob] = $prt[1];
    }
  }

########
     $njob++;
 } 

########### jobs in HELD

 @joblist = ();

 @joblist = `crs_job -stat | grep HELD` ;

    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
#     print $wrd[0],"   ",$wrd[1], "\n";

     $jbId[$njob] = $wrd[0];
     $jbId[$njob] = substr($wrd[0],0,-1) + 0;
     $jbstate[$njob] = "HELD";  
     $jberror[$njob] = 0;
     $Tperror = 0;
     $jbtri[$njob] = 0; 
     $jbstart[$njob] ="0000-00-00 00:00:00";

     @errlines = ();
     @errlines = `crs_job -long $jbId[$njob] | grep Error`; 

   foreach my $erline (@errlines) {
     chop $erline ;
#   print $erline, "\n";
     if ( $erline =~ /Error/ ) {

     @prt = ();
     @prt = split (" ", $erline);

#  print "Error line : ", $pt[1],"  ", $pt[2],"  ",$pt[3], "\n";

     $Tperror = $prt[2];
     $Tperror =~ s/://g;

      }
    }
     $jberror[$njob] =  $Tperror;


    @jobnode = ();
    @jobnode = `crs_job -long $jbId[$njob] | grep Machine`;

   foreach my $jnode (@jobnode) {
     chop $jnode ;
#     print $jnode, "\n";
    if ( $jnode =~ /Machine/ ) {
       @prt = ();
       @prt = split(" ", $jnode) ;
     $jbnode[$njob] = $prt[1];
    }
  }
######

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
       chop $wrd[8]; 
       $jbfiles[$ii] = $wrd[8];
       $jbfiles[$ii] =~ s/MuDst.root'/daq/g; 
       @spl = (); 
       @spl = split("_", $wrd[8]) ; 
       $jbstreams[$ii] = $spl[1]; 

       
#       print "Inserting into table values:  ", $jbstate[$ii]," % ",$jbId[$ii]," % ",$jbtrigs[$ii]," % ",$prodtags[$ii]," % ",$runId[$ii]," % ",$jbstreams[$ii]," % ",$jbfiles[$ii], "\n";


     &fillTable();

      }
     }
###########  insert table
}

    print "Data inserted into the table for $thisdate","\n";

     $sql= "update $crsJobStatusT set flag = 'Done' where runDate = '$thisdate' and flag = 'Start' ";
       $dbh->do($sql) || die $dbh->errstr;

    for ($ii=0; $ii<$nn; $ii++) {
    
     $sql= "delete from $crsJobStatusT where runDate = '$prevdate[$ii]' ";
        $dbh->do($sql) || die $dbh->errstr;

	print "Data deleted for datetime = ", $prevdate[$ii], "\n";

    }

 &StcrsdbDisconnect();

exit;


#################################################################################################

  sub fillTable {

 $sql="insert into $crsJobStatusT set ";
 $sql.="jobId='$jbId[$ii]',";
 $sql.="status='$jbstate[$ii]',";
 $sql.="prodtag='$prodtags[$ii]',";
 $sql.="trigset='$jbtrigs[$ii]',";
 $sql.="runnumber='$runId[$ii]',";
 $sql.="filename='$jbfiles[$ii]',";
 $sql.="stream='$jbstreams[$ii]',";
 $sql.="error='$jberror[$ii]',";
 $sql.="nodeID='$jbnode[$ii]',";
 $sql.="startTime='$jbstart[$ii]',";
 $sql.="attempt='$jbtri[$ii]',";
 $sql.="runDate='$thisdate' "; 
   print "$sql\n" if $debugOn;
#   $rv = $dbh->do($sql) || die $dbh->errstr;
    $dbh->do($sql) || die $dbh->errstr;
  }

##################################################################################################
sub StcrsdbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StcrsdbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
