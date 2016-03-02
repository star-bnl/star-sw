#!/usr/local/bin/perl -w
#
# CRSJobstream.pl - monitor number of different stream jobs on the farm in MAIN-EXEC stage
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
$crsJobStreamsT = "crsJobStreamsY10";

 my @statlist = ();
 my @joblist  = ();

 @statlist = `jobstat | grep MAIN-EXEC`;
 
 
 my $year;
 my $mon = 0;
 my $mday = 0;
 my $hour = 0;
 my $min = 0;
 my $sec = 0;
 my $thisday ;

my $Njb = 0;
my $Nphysics = 0;
my $Nht = 0;
my $Nhlt = 0;
my $Ngamma = 0;
my $Nfmsfast = 0;
my $Nminbias = 0;
my $Nmtd = 0;
my $Nmonitor = 0;
my $Npmdftp = 0;
my $Nupsilon = 0;
my $Nupc = 0;
my $Nzerobias = 0;
my $Nwb = 0;
my $Natomcules = 0;

my @prt = ();

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

 $Njb = scalar(@statlist);

  foreach my $jobname (@statlist) {
     chop $jobname ;
#   print  $jobname, "\n";

     if ($jobname =~ /st_physics/) {
	$Nphysics++;
	} elsif ($jobname =~ /st_ht/) {
        $Nht++; 
	} elsif ($jobname =~ /st_hlt/) {         
        $Nhlt++; 
	} elsif ($jobname =~ /st_gamma/) {       
        $Ngamma++;
	} elsif ($jobname =~ /st_fmsfast/) {       
        $Nfmsfast++;
	} elsif ($jobname =~ /st_minbias/) { 
         $Nminbias++;
	} elsif ($jobname =~ /st_mtd/) { 
         $Nmtd++;
	} elsif ($jobname =~ /st_monitor/) { 
         $Nmonitor++;
	} elsif ($jobname =~ /st_pmdftp/) { 
         $Npmdftp++;
	} elsif ($jobname =~ /st_upsilon/) { 
         $Nupsilon++;
	} elsif ($jobname =~ /st_upc/) { 
         $Nupc++;
	} elsif ($jobname =~ /st_zerobias/) {        
         $Nzerobias++;
 	} elsif ($jobname =~ /st_W/) {        
         $Nwb++;
 	} elsif ($jobname =~ /st_atomcules/) {        
         $Natomcules++;
	}
    }


      &fillTable();


   &StcrsdbDisconnect();

exit;


#################################################################################################

  sub fillTable {

 $sql="insert into $crsJobStreamsT set ";
 $sql.="Njobs='$Njb',";
 $sql.="physics='$Nphysics',";
 $sql.="ht='$Nht',";
 $sql.="hlt='$Nhlt',"; 
 $sql.="gamma='$Ngamma',";
 $sql.="fmsfast='$Nfmsfast',";
 $sql.="minbias='$Nminbias',";
 $sql.="mtd='$Nmtd',";
 $sql.="monitor='$Nmonitor',";
 $sql.="pmdftp='$Npmdftp',";
 $sql.="upsilon='$Nupsilon',";
 $sql.="atomcules='$Natomcules',";
 $sql.="upc='$Nupc',";
 $sql.="Wbs='$Nwb',";
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
