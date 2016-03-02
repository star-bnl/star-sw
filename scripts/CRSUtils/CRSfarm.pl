#!/usr/local/bin/perl -w
#
# L. Didenko
###############################################################################

 use Mysql;
 use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";


# Tables
$crsJobStatusT = "crsJobStatusY10";

 my @statlist = ();
 my @joblist  = ();

 @statlist = `farmstat`;
 
 my $year;
 my $mon = 0;
 my $mday = 0;
 my $hour = 0;
 my $min = 0;
 my $sec = 0;
 my $thisday ;

my $Nsubm = 0;
my $Nsubmfail = 0;
my $Nstart = 0;
my $Nimportw = 0;
my $Nimporth = 0;
my $Nsleep = 0;
my $Nexe = 0;
my $Nexportw = 0;
my $Nexporth = 0;
my $Nexportu = 0;
my $Ndone = 0;
my $Nerror = 0;
my $Nhpexport = 0;
my $Nhpimport = 0;
my $Nhprespon = 0;
my $Nhperr = 0;
my $Ntimeout = 0;
my $Nhpbusy = 0;
my $Nfatal = 0;
my @prt = ();
my @wrd = ();


 ($sec,$min,$hour,$mday,$mon,$yr) = localtime;


    $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };

  $year = $yr + 1900;

  $thisday = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

 print $thisday, "\n";

   &StcrsdbConnect();

  foreach $line (@statlist) {
     chop $line ;
#   print  $line, "\n";
    @prt = ();
    @prt = split (" ", $line);
     if ($prt[0] eq "SUBMITTED") {
	 $Nsubm =  $prt[1];
	} elsif ($prt[0] eq "STARTED") {
        $Nstart =  $prt[1]; 
	} elsif ($prt[0] eq "MAIN-IMPORT-WAITING") {         
        $Nimportw =  $prt[1]; 
	} elsif ($prt[0] eq "MAIN-IMPORT-HPSS") {       
        $Nimporth =  $prt[1];
	} elsif ($prt[0] eq "MAIN-SLEEP") {       
        $Nsleep =  $prt[1];
	} elsif ($prt[0] eq "MAIN-EXEC") { 
         $Nexec =  $prt[1];
	} elsif ($prt[0] eq "MAIN-EXPORT-UNIX") { 
         $Nexportu =  $prt[1];
	} elsif ($prt[0] eq "MAIN-EXPORT-WAITING") { 
         $Nexportw =  $prt[1];
	} elsif ($prt[0] eq "MAIN-EXPORT-HPSS") { 
         $Nexporth  =  $prt[1];
	} elsif ($prt[0] eq "DONE") { 
         $Ndone =  $prt[1];
	} elsif ($prt[0] eq "SUBMIT_FAILED") { 
         $Nsubmfail =  $prt[1];
	} elsif ($prt[0] eq "ERROR") {        
         $Nerror =  $prt[1];
 	} elsif ($prt[0] eq "FATAL") {        

	    if ($prt[1] >= 0 ) {
         $Nfatal =  $prt[1];
            }else{
            $Nfatal = 0;
         }
	}
 }

      @joblist = `crs_job -stat_show_problem | grep ERROR` ;

    foreach my $erline (@joblist) {
     chop $erline ;
#      print $erline, "\n";

      @wrd = ();
      @wrd = split (" ", $erline);

    if ( $wrd[1] eq "hpss_export_failed" ) {
      $Nhpexport++;
  }elsif($wrd[1] eq "pftp_get_failed") { 
      $Nhpimport++;

  }elsif($wrd[1] eq "no_response_from_hpss_server") {      
      $Nhprespon++;
  }elsif($wrd[1] eq "hpss_stage_request_timed_out") {  
      $Ntimeout++;
  }elsif($wrd[1] eq "hpss_request_submission_timed_out") {  
      $Ntimeout++;
  }elsif($wrd[1] eq "hpss_busy") {  
      $Nhpbusy++;
  }elsif($wrd[1] =~ /hpss_error/) {
     $Nhperr++;

     }else{
   }

 }


      &fillTable();

#     print $Nsubm,"   ",$Nstart,"   ",$Nimportw,"   ",$Nimporth,"   ",$Nexec,"   ",$Nexportw,"   ",$Nexporth,"   ",$Ndone,"   ",$Nerror,"   ",$Nfatal, "\n";
   &StcrsdbDisconnect();

exit;


#################################################################################################

  sub fillTable {

 $sql="insert into $crsJobStatusT set ";
 $sql.="submitted='$Nsubm',";
 $sql.="submitFailed='$Nsubmfail',";
 $sql.="started='$Nstart',";
 $sql.="importWaiting='$Nimportw',"; 
 $sql.="importHPSS='$Nimporth',";
 $sql.="sleep='$Nsleep',";
 $sql.="executing='$Nexec',";
 $sql.="exportWaiting='$Nexportw',";
 $sql.="exportHPSS='$Nexporth',";
 $sql.="exportUNIX='$Nexportu',";
 $sql.="done='$Ndone',";
 $sql.="error='$Nerror',";
 $sql.="hpss_export_failed='$Nhpexport',";
 $sql.="hpss_import_failed='$Nhpimport',";
 $sql.="hpss_no_response='$Nhprespon',";
 $sql.="hpss_timeout='$Ntimeout',";
 $sql.="hpss_busy='$Nhpbusy',";
 $sql.="hpss_error='$Nhperr',";
 $sql.="fatal='$Nfatal',";
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
