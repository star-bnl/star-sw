#! /opt/star/bin/perl -w
#
# 
#
# 
#
# L. Didenko 
######################################################################
#
# dbProdOpt.pl     
#
# Create db for production chain options and library versions
#
#
use Net::FTP;
use Class::Struct;
use File::Basename;
use Class::Struct;

require "/afs/rhic/star/packages/SL99i/mgr/dbProdOptSetup.pl";
#require "dbProdOptSetup.pl";

my $debugOn = 0;


 my @prodPeriod   = ("prod4", "prod5");
 my @libVer = ("SL99g_7", "SL99g_8");
 my @optChain = ("off_tdaq_HalfField_tpc_global_dst_event_analysis_tree_xout",
  "off_tdaq_FieldOff_tpc_global_dst_event_analysis_tree_xout", 
  "off_tdaq_FieldOn_tpc_global_dst_event_analysis_tree_xout");


 my $nChain = scalar(@optChain);
 my $nSeries = scalar(@prodPeriod);
 my $nlibVer = scalar(@libVer);  

## initialize variables

my $prodSer = "n\/a";
my $evtType = 0;
my $lib_ver = "n\/a";
my $mchain = "n\/a";
my $mfield1 = "n\/a";
my $mfield2 = "n\/a";

my $ii = 0;
my $ik = 0;


&StDbProdOptionsConnect();

# need to clear the table first here
$sql="delete from $ProdOptionsT";
$cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;
#

## filling DB table

  for ($ii = 0; $ii<$nChain; $ii++) {
      $prodSer = $prodPeriod[0];
      $lib_ver = $libVer[0];
      $mchain = $optChain[$ii];
      $evtType = 999;
  print "filling prodOption table\n";
     &fillTable();

} 
    

 &StDbProdOptionsDisconnect();
 exit; 

#==========================================================================
sub fillTable {

      $sql="insert into $ProdOptionsT set ";
      $sql.="prodSeries='$prodSer',";
      $sql.="eventType='$evtType',";
      $sql.="libVersion='$lib_ver',";
      $sql.="chainOpt='$mchain',"; 
      $sql.="field1='$mfield1',";
      $sql.="field2='$mfield2',";

      print "$sql\n" if $debugOn;
      $rv = $dbh->do($sql) || die $dbh->errstr;

    }



