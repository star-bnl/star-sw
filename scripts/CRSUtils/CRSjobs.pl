#!/usr/bin/env perl
#
#  $Id:
#
#  $Log:   CRSJobs.pl - monitoring of CRS jobs
#           
#  L.Didenko
###############################################################################

use Mysql;

 require "/afs/rhic/star/packages/scripts/dbProdSetup.pl";

my @maifile;
my $mail_line;
my $status_line;
my $jbStat = "n/a";
my @parts;
my $nodeID = "n/a";
my $mynode; 
my @wrd;
my %nodeCrCount = ();
my %nodeStCount = ();
my %nodeAbCount = ();
my %nodeDnCount = ();
my %nodeFNFCount = ();
my %nodeQuFaCount = ();
my %nodeTrFaCount = ();
my %nodeMsFaCount = ();
my %nodeDbFaCount = ();

my $outname;
my $outfile;

my @ndCrCount = ();
my @ndAbCount = ();
my @ndStCount = (); 
my @ndDnCount = ();
my @ndFNFCount = ();
my @ndQuFaCount = ();
my @ndTrFaCount = ();
my @ndMsFaCount = ();
my @ndDbFaCount = ();
my @nodeList = ();
my $nodenum = 0;  
my %nodeHash = ();
my $nodeID;
my $ii = 0;
   
   for (my $ik=0; $ik< 211; $ik++) {

       $nodenum = 6001 + $ik;
       $nodeID = "rcrs" .$nodenum. ".rcf.bnl.gov";
       $nodeList[$ik] = $nodeID ;
#      print $nodeList[$ik], "\n"; 
       $nodeHash{$nodeID} = $ik;
#       print  $nodeHash{$nodeID}, "\n";
  };
       $nodeList[211] = "n/a";
       $nodeHash{"n/a"} = 211;

my $eachNode;


my $today;

($sec,$min,$hour,$mday,$mon) = localtime;

my $year = "2005";
   $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };

 $thisday = $year."-".$mon."-".$mday; 
 $today = $thisday;
# print $thisday, "\n";
$outname = "mail" . "_" .$thisday . "_" . "out";

 $outfile = "/star/u/starreco/" . $outname;


 print $outfile, "\n";

open (MAILFILE, $outfile ) or die "cannot open $outfile: $!\n";

 @mailfile = <MAILFILE>;

  for ( $ll = 0; $ll <= scalar(@nodeList); $ll++) {   
 
       $ndCrCount[$ll]  = 0;
       $ndAbCount[$ll]  = 0;
       $ndStCount[$ll]  = 0;
       $ndDnCount[$ll]  = 0;
       $ndFNFCount[$ll] = 0;
       $ndQuFaCount[$ll] = 0;
       $ndTrFaCount[$ll] = 0;
       $ndMsFaCount[$ll] = 0;
       $ndDbFaCount[$ll] = 0;
     };

   &StDbProdConnect();

  foreach $mail_line (@mailfile) {
     chop $mail_line ;
    $jbStat = "n/a";
     if ($mail_line =~ /JobInfo/ ) {
      @wrd = split ("%", $mail_line);
      $nodeID = $wrd[2];
      if( $nodeID =~ /rcrs/) {
     chop $nodeID;
      $nodeID =~ s/^\ *//g;
   }else {
      $nodeID = "n/a";
   }
      $jbStat = $wrd[1];
      if(! $jbStat ) {$jbStat = "n/a"};

     if(!defined($nodeID)) {$nodeID = "n/a"};       
      $ii = $nodeHash{$nodeID};

#  print $nodeID,"  ", $jbStat, "\n";

       if ($jbStat =~ /crashed/) {
        $ndCrCount[$ii]++;
     }
      elsif ($jbStat =~ /aborted/) {
        $ndAbCount[$ii]++;  
     }
     elsif ($jbStat =~ /staging failed/) {
         $nodeID = "n/a";
       $ii = $nodeHash{$nodeID};     
         $ndStCount[$ii]++;
     }
     elsif ($jbStat =~ /file not found/) {
         $ndFNFCount[$ii]++;
     }
      elsif ($jbStat =~ /queuing failed/) {
         $ndQuFaCount[$ii]++;
       }
      elsif ($jbStat =~ /transfer failed/) {
         $ndTrFaCount[$ii]++;
       }
      elsif ($jbStat =~ /msg. failed/) {
         $ndMsFaCount[$ii]++;
       }
      elsif ($jbStat =~ /db failed/) {
         $ndDbFaCount[$ii]++;
       }
     elsif ($jbStat =~ /done/) {
         $ndDnCount[$ii]++;
       }
   } 
 }
 
close (MAILFILE);

 my $dbnode = "none";
 my $dbDate = "none";

for ($ll = 0; $ll < scalar(@nodeList); $ll++) {
      $mynode = $nodeList[$ll];
#      print "Check node name  ", $mynode, "\n";
      $nodeCrCount{$mynode} = $ndCrCount[$ll];
      $nodeAbCount{$mynode} = $ndAbCount[$ll];
      $nodeStCount{$mynode} = $ndStCount[$ll]; 
      $nodeDnCount{$mynode} = $ndDnCount[$ll];       
      $nodeFNFCount{$mynode} = $ndFNFCount[$ll];
      $nodeQuFaCount{$mynode} = $ndQuFaCount[$ll];      
      $nodeTrFaCount{$mynode} = $ndTrFaCount[$ll];  
      $nodeMsFaCount{$mynode} = $ndMsFaCount[$ll];  
      $nodeDbFaCount{$mynode} = $ndDbFaCount[$ll];       

      $dbnode = "none";
      $dbDate = "none";
      
  $sql="SELECT nodeName, mdate FROM $crsStatusT WHERE nodeName = '$mynode' AND mdate = '$thisday' ";    

       $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};
  
   for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];
       print "$fname = $fvalue\n" if $debugOn;
#       print "$fname = $fvalue\n";

       $dbnode = $fvalue     if( $fname eq 'nodeName');
       $dbDate = $fvalue     if( $fname eq 'mdate');
     }
   }      
 
     if( $dbnode eq "none" and $dbDate eq "none" ) {
   print "Filling new entries  ",$thisday,"  %  ",$mynode,"  %  ",$nodeCrCount{$mynode}, "  %  ",$nodeAbCount{$mynode}, "  %  ",$nodeStCount{$mynode}, "  %  ",$nodeDnCount{$mynode}, "  %  ",$nodeFNFCount{$mynode},"  %  ", $nodeQuFaCount{$mynode}, "\n";

  &fillTable();
     }else{
#   print "Updated  ",$thisday,"  %  ",$mynode,"  %  ",$nodeCrCount{$mynode}, "  %  ",$nodeAbCount{$mynode}, "  %  ",$nodeStCount{$mynode}, "  %  ",$nodeDnCount{$mynode}, "  %  ",$nodeFNFCount{$mynode},"  %  ", $nodeQuFaCount{$mynode}, "\n";
  &updateTable(); 
   }
 }     
 
   &StDbProdDisconnect();

exit;

#######################################################################

  sub fillTable {

 $sql="insert into $crsStatusT set ";
 $sql.="nodeName='$mynode',";
 $sql.="crashedJobs='$nodeCrCount{$mynode}',";
 $sql.="abortedJobs='$nodeAbCount{$mynode}',"; 
 $sql.="stagingFailed='$nodeStCount{$mynode}',";
 $sql.="doneJobs='$nodeDnCount{$mynode}',";
 $sql.="fileNotFound='$nodeFNFCount{$mynode}',";
 $sql.="queuingFailed='$nodeQuFaCount{$mynode}',";
 $sql.="transferFailed='$nodeTrFaCount{$mynode}',";
 $sql.="msgFailed='$nodeMsFaCount{$mynode}',";
 $sql.="dbFailed='$nodeDbFaCount{$mynode}',";
 $sql.="mdate='$thisday' "; 
    print "$sql\n" if $debugOn;
  $rv = $dbh->do($sql) || die $dbh->errstr;
   }

#######################################################################

  sub updateTable {

 $sql="update $crsStatusT set ";
 $sql.="crashedJobs='$nodeCrCount{$mynode}',";
 $sql.="abortedJobs='$nodeAbCount{$mynode}',"; 
 $sql.="stagingFailed='$nodeStCount{$mynode}',";
 $sql.="doneJobs='$nodeDnCount{$mynode}',";
 $sql.="fileNotFound='$nodeFNFCount{$mynode}',";
 $sql.="queuingFailed='$nodeQuFaCount{$mynode}',";
 $sql.="transferFailed='$nodeTrFaCount{$mynode}',";
 $sql.="msgFailed='$nodeMsFaCount{$mynode}',";
 $sql.="dbFailed='$nodeDbFaCount{$mynode}' ";
 $sql.=" WHERE nodeName = '$mynode' AND  mdate = '$thisday' ";
    print "$sql\n" if $debugOn;
   $rv = $dbh->do($sql) || die $dbh->errstr;
   }
