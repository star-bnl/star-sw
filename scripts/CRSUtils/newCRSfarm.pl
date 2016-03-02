#!/usr/local/bin/perl -w
#
#  $Id:
#
#  L. Didenko
#
#  $Log: Script to monitor jobs status and errors using new CRS software interface
#
###############################################################################

use DBI;
use File::Basename;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";


# Tables
$crsJobStatusT = "newcrsJobState";

 my $prodtag = $ARGV[0];
 my $pflag = $ARGV[1];
 my $caltag;
 my @prt = ();

 my $JOBDIR = "/star/u/starreco/".$prodtag."/requests/daq/";
 my $archdir;
 my $hpssfail = $JOBDIR."jobs_lostfiles";
 my $resubdir = $JOBDIR."jobs_rerun";

 if( $pflag eq "reco" ) {

 $JobStatusT = "JobStatus2013";
 $archdir = $JOBDIR."archive";

 }elsif( $pflag =~ /calib/ ) {

 $JobStatusT = "CalibJobStatus";
  @prt = split ("_",$pflag);
  $caltag = $prt[1];
  $archdir = $JOBDIR."archive_calib";

   }

 my @statlist = ();
 my @joblist  = ();

 @statlist = `/usr/bin/crs_summary`;
 
 my $year;
 my $mon = 0;
 my $mday = 0;
 my $hour = 0;
 my $min = 0;
 my $sec = 0;
 my $thisday ;

my $Ncreate = 0;
my $Nqueued = 0;
my $Nstage = 0;
my $Nsubm = 0;
my $Nimport = 0;
my $Nrun = 0;
my $Nexport = 0;
my $Ndone = 0;
my $Nerror = 0;
my $Nkill = 0;
my $Nheld = 0;

my $NFprestage = 0;
my $NFexport = 0;
my $NFimport = 0;
my $NFretry = 0;
my $Nioerror = 0;
my $NFcondor = 0;
my $NFjexec = 0;
my $Tperror = 0;
my $hpsserr;
my @joberr = ();
my @jid = ();
my @jobname = ();
my @fullname = ();
my @inpfile = ();
my $njob = 0;
my $fname;
my @jobFname = ();
my @jobnode = ();
my @jbnode = ();
my $conderr;

my $infile;
my $jbfile;

my @prt = ();
my @wrd = ();
my @pt = ();

 ($sec,$min,$hour,$mday,$mon,$yr) = localtime;


    $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };

 $year = $yr + 1900;

 $thisday = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;


 print "-------------------------------------","\n";
 print $thisday, "\n";

   &StcrsdbConnect();

  foreach my $line (@statlist) {
     chop $line ;

    next if($line =~ /---/ or $line =~ /Status/ );
   print  $line, "\n";
   @prt = ();
   @prt = split (" ", $line);

#   print "Check parts  ", ,$prt[1],"  ",$prt[3],"\n";

     if ($prt[3] eq "SUBMITTED") {
	 $Nsubm =  $prt[1];
	} elsif ($prt[3] eq "CREATED") {
        $Ncreate =  $prt[1]; 
	} elsif ($prt[3] eq "QUEUED") {         
        $Nqueued =  $prt[1]; 
	} elsif ($prt[3] eq "STAGING") {       
        $Nstage =  $prt[1];
	} elsif ($prt[3] eq "IMPORTING") {       
        $Nimport =  $prt[1];
	} elsif ($prt[3] eq "RUNNING") {       
        $Nrun =  $prt[1];
	} elsif ($prt[3] eq "EXPORTING") {       
        $Nexport =  $prt[1];
	} elsif ($prt[3] eq "DONE") { 
        $Ndone =  $prt[1];
	} elsif ($prt[3] eq "ERROR") {        
        $Nerror =  $prt[1];
 	} elsif ($prt[3] eq "KILLED") {        
        $Nkill =  $prt[1];
	} elsif ($prt[3] eq "HELD") {        
        $Nheld =  $prt[1];

	`crs_job -reset -s HELD`;
	`crs_job -submit -s CREATED`;
  
        }
   }

   `crs_job -destroy -f -s DONE`; 

  @joblist = `crs_job -stat | grep ERROR` ;

   
    foreach my $jline (@joblist) {
     chop $jline ;
#     print $jline, "\n";
     @wrd = ();
     @wrd = split (" ", $jline);
     print $wrd[0],"   ",$wrd[1], "\n";

     $jid[$njob] = $wrd[0];
     $jid[$njob] = substr($wrd[0],0,-1) + 0;

#    print "Job id = ",$jid[$njob], "\n";

    @joberr = ();
    @joberr = `crs_job -long $jid[$njob] | grep Error`;

    @inpfile = ();
    @inpfile = `crs_job -long $jid[$njob] | grep starsink`;

    @jobnode = ();
    @jobnode = `crs_job -long $jid[$njob] | grep Machine`;


    foreach my $jnode (@jobnode) {
     chop $jnode ;
#     print $jnode, "\n";
    if ( $jnode =~ /Machine/ ) {
       @prt = ();
       @prt = split(" ", $jnode) ;
    $jbnode[$njob] = $prt[1];

     }
  } 

    foreach my $fline (@inpfile) {
     chop $fline ;
#     print $fline, "\n";
    if ( $fline =~ /starsink/ ) {
       @prt = ();
       @prt = split("starsink", $fline) ;
#       print "Check line with filename : ", $prt[0],"  ", $prt[1],"\n";
       $fname = $prt[1];
       $fname =~ s/.daq'//g;
       $jobname[$njob] = basename($fname); 
       $fullname[$njob] = $archdir."/*".$jobname[$njob]; 

   if( $pflag eq "reco" ) {

       $jobFname[$njob] = "%".$prodtag."_".$jobname[$njob];
   }elsif( $pflag =~ /calib/ ) {
       $jobFname[$njob] = "%".$prodtag."_".$caltag."_".$jobname[$njob];
         } 
       }
      }    #foreach my $fline


    foreach my $erline (@joberr) {
     chop $erline ;
#   print $erline, "\n";
     if ( $erline =~ /Error/ ) {

     @pt = ();
     @pt = split (" ", $erline);

#  print "Error line : ", $pt[1],"  ", $pt[2],"  ",$pt[3], "\n";
     
     $Tperror = $pt[2];
     $Tperror =~ s/://g;

   print "Job id, node and error number =  ", $jid[$njob],"   ",$jbnode[$njob],"   ",$Tperror,"\n";

      if($Tperror == 10) {
	 $NFcondor++;

     `crs_job -reset $jid[$njob]`;
     `crs_job -submit $jid[$njob]`;

    $conderr = "error_".$Tperror;

    $sql="update $JobStatusT set crsError = '$conderr', nodeID = '$jbnode[$njob]', crsjobId = '$jid[$njob]' where jobfileName like '$jobFname[$njob]' ";

    $rv = $dbh->do($sql) || die $dbh->errstr;


  print "Job   ",$jid[$njob],"   was reset due to condor problem","\n";

    }elsif($Tperror == 20) {
        $NFprestage++;
    }elsif($Tperror == 30) {
        $NFretry++;  
    }elsif($Tperror == 40) {
        $NFimport++;  
    }elsif($Tperror == 50) {
        $NFjexec++;  

     `crs_job -reset $jid[$njob]`;
     `crs_job -submit $jid[$njob]`;

    $conderr = "error_".$Tperror;

    $sql="update $JobStatusT set crsError = '$conderr', nodeID = '$jbnode[$njob]', crsjobId = '$jid[$njob]' where jobfileName like '$jobFname[$njob]' ";

    $rv = $dbh->do($sql) || die $dbh->errstr;


  print "Job   ",$jid[$njob],"   was reset due to execution error","\n";

    }elsif($Tperror == 60) {
        $NFexport++; 
 
     `crs_job -kill -f $jid[$njob]`; 
     `crs_job -destroy -f $jid[$njob]`; 

    $hpsserr = "error_".$Tperror;

    $sql="update $JobStatusT set crsError = '$hpsserr', nodeID = '$jbnode[$njob]', crsjobId = '$jid[$njob]'  where jobfileName like '$jobFname[$njob]' ";

    $rv = $dbh->do($sql) || die $dbh->errstr;

    print "Job   ",$jid[$njob],"   was killed, failed due to HPSS export problem","\n";

    }elsif($Tperror == 70) {
        $Nioerror++;  

     `crs_job -reset $jid[$njob]`;
     `crs_job -submit $jid[$njob]`;

    $conderr = "error_".$Tperror;

    $sql="update $JobStatusT set crsError = '$conderr', nodeID = '$jbnode[$njob]', crsjobId = '$jid[$njob]' where jobfileName like '$jobFname[$njob]' ";

    $rv = $dbh->do($sql) || die $dbh->errstr;

  print "Job   ",$jid[$njob],"   was reset due to I/O error","\n";

     }

     if( $Tperror == 20 or $Tperror == 30 or $Tperror == 40 ) {

     `crs_job -kill -f $jid[$njob]`; 
     `crs_job -destroy -f $jid[$njob]`; 


    print "Jobid, nodeID, input filename, error number are : ",$njob,"  ",$jid[$njob],"  ",$jbnode[$njob],"  ",$jobname[$njob],"   ",$Tperror,"\n";

    print "Job   ",$jid[$njob],"   was killed, failed due to HPSS import problem","\n";

    $hpsserr = "error_".$Tperror;

    $sql="update $JobStatusT set inputHpssStatus = '$hpsserr', nodeID = '$jbnode[$njob]', crsjobId = '$jid[$njob]'  where jobfileName like '$jobFname[$njob]' and jobStatus = 'n/a' ";

    $rv = $dbh->do($sql) || die $dbh->errstr;


     `mv $fullname[$njob] $hpssfail \n`;

    print "Jobfilename  ",$fullname[$njob]," was moved to ", $hpssfail, "\n";  

       }   
######
     }

    }   #foreach  my $erline 

#####
     $njob++;

   }   #foreach my $jline


     &fillTable();

    print "Number of jobs state : ","Ncreate = ", $Ncreate,"   ","Nqueued = ",$Nqueued,"   ","Nstage = ",$Nstage,"   ","Nsubm = ", $Nsubm,"   ","Nimport = ",$Nimport,"   ","Nrun = ",$Nrun,"   ","Nexport = ",$Nexport,"   ","Ndone = ",$Ndone,"   ","Nerror = ",$Nerror,"   ","Nkill = ",$Nkill,"   ","Nheld = ",$Nheld, "\n";

   print "Number of errors : ","NFcondor = ",$NFcondor,"   ","NFprestage = ",$NFprestage,"   ","NFimport = ",$NFimport,"   ","NFexport = ",$NFexport,"   ","NFretry = ",$NFretry,"   ","NFjexec = ",$NFjexec,"   ","Nioerror = ",$Nioerror, "\n";

   &StcrsdbDisconnect();

exit;


#################################################################################################

  sub fillTable {

 $sql="insert into $crsJobStatusT set ";
 $sql.="created='$Ncreate',"; 
 $sql.="submitted='$Nsubm',";
 $sql.="staging='$Nstage',";
 $sql.="queued='$Nqueued',";
 $sql.="importing='$Nimport',"; 
 $sql.="running='$Nrun',";
 $sql.="exporting='$Nexport',";
 $sql.="killed='$Nkill',";
 $sql.="held='$Nheld',";
 $sql.="done='$Ndone',";
 $sql.="error='$Nerror',";
 $sql.="prestaging_failed='$NFprestage',";
 $sql.="hpss_export_failed='$NFexport',";
 $sql.="hpss_import_failed='$NFimport',";
 $sql.="hpss_retry_failed='$NFretry',";
 $sql.="job_exec_failed='$NFjexec',";
 $sql.="io_error='$Nioerror',";
 $sql.="condor_failed='$NFcondor',";
 $sql.="sdate='$thisday' "; 
#   print "$sql\n" if $debugOn;
    # $rv = $dbh->do($sql) || die $dbh->errstr;
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
