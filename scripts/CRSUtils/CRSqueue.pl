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
$crsQueueT = "crsQueueY10";

 my @statlist = ();

 @statlist = `crs_job -show_queues`;
 
 my $year;
 my $mon = 0;
 my $mday = 0;
 my $hour = 0;
 my $min = 0;
 my $sec = 0;
 my $timestamp ;

my @queueRun = ();
my @queueAvl  = ();
my @Rqueue = ();

my @prt = ();


 ($sec,$min,$hour,$mday,$mon,$yr) = localtime;


    $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };

 $year = $yr + 1900;

  $timestamp = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

 print $timestamp, "\n";

   &StcrsdbConnect();
 
# GVB: There is a bug obviated by not initializing
# these: the code below assumes the 2nd and 3rd reported
# queues are 3 and 4; presently they are actually 4 and 5
  for (my $ni = 0; $ni < 6; $ni++) {
    $queueRun[$ni] = 0;
    $Rqueue[$ni] = 0;
  }

my $nn = 0;
my $nk = 0;

  foreach $line (@statlist) {
     chop $line ;
#  print  $line, "\n";
     next if( $line =~ /queue/);
    @prt = ();
    @prt = split (" ", $line);
     if( $nn == 0) {
     $nk = $nn;
 }else{
     $nk = $nn +2;
 }
    $queueAvl[$nk] = $prt[1]; 
    $queueRun[$nk] = $prt[2]; 
     if($queueAvl[$nk] >=1 ) {
	 $Rqueue[$nk] = ($queueRun[$nk]/$queueAvl[$nk])*100.0 ;
     } else {
     $Rqueue[$nk] = 0; 
  }
#    print $nn,"   ", $nk, "  All slots:  ", $queueAvl[$nk], "  Busy: ", $queueRun[$nk], "  Ratio ", $Rqueue[$nk], "\n";
    $nn++;

 }

   &fillTable();

   &StcrsdbDisconnect();

exit;


#################################################################################################

sub fillTable {

    $sql="insert into $crsQueueT set ";
    $sql.="queue0='$queueRun[0]',";
    $sql.="queue1='$queueRun[1]',";
    $sql.="queue2='$queueRun[2]',"; 
    $sql.="queue3='$queueRun[3]',";
    $sql.="queue4='$queueRun[4]',";
    $sql.="queue5='$queueRun[5]',";
    $sql.="Rqueue0='$Rqueue[0]',";
    $sql.="Rqueue1='$Rqueue[1]',";
    $sql.="Rqueue2='$Rqueue[2]',"; 
    $sql.="Rqueue3='$Rqueue[3]',";
    $sql.="Rqueue4='$Rqueue[4]',";
    $sql.="Rqueue5='$Rqueue[5]',";
    $sql.="sdate='$timestamp' "; 

    #  print "$sql\n" if $debugOn;
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
