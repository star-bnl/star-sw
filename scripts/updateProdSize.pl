#! /usr/local/bin/perl -w
#
#
# $Id: 
# 
# $Log: updateProdSize.pl
#
# script to update productionSize database with MuDst and daq files sizes.
# Author L. Didenko
#
############################################################################################################


use DBI;
use Class::Struct;
use Time::Local;
use File::Basename;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";


my $FileCatalogT = "FileCatalog2014";
my $ProdSizeT = "ProductionSize"; 
my $JobStatusT = "JobStatus2014";


my $trigname = $ARGV[0];
my $prodTag = $ARGV[1];
my @mufiles = ();
my @musizes = ();
my @muevents = ();
my @daqevents = ();
my @daqfiles  = ();
my @daqsizes  = ();
my @runnum = ();
my @crtime = ();
my @strtime = ();
my $nj = 0;
my @dbmufiles = ();
my @dbmusize = ();
my $nn = 0;
my $dqfile;
my $basefile ;
my $maxtime ;

 &StDbConnect();


   $sql="SELECT date_format(max(createtime), '%Y-%m-%d') FROM $ProdSizeT WHERE Trigset = '$trigname' and prodtag = 'prodTag' and filename like '%.MuDst.root' ";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;

     my $crHTime = $cursor->fetchrow ;
      $cursor->finish;

   $maxtime = $crHTime;
$maxtime = "2015-03-31";

    $sql="SELECT distinct runID, fName, size, Nevents, createtime FROM $FileCatalogT where trigset = '$trigname' and path like '%$prodTag%' and fName like '%.MuDst.root' and createTime >= ' $maxtime'  order by runID ";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute();

    while( @fields = $cursor->fetchrow() ) {

    $runnum[$nj]   = $fields[0];
    $mufiles[$nj]  = $fields[1];
    $musizes[$nj]  = $fields[2];
    $muevents[$nj] = $fields[3];
    $crtime[$nj]   = $fields[4];
    $nj++;
       }
    $cursor->finish();

  
  for ( my $ik = 0; $ik < $nj; $ik++ ) {

      $basefile = basename("$mufiles[$ik]",".MuDst.root");
      $dqfile = $basefile.".daq" ;
      $daqfiles[$ik] = $dqfile;

    $sql="SELECT distinct fName, size, Nevents FROM $FileCatalogT where trigset = '$trigname' and fName = '$dqfile' ";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute();

    while( @fields = $cursor->fetchrow() ) {
 
    $daqsizes[$ik] = $fields[1];
    $daqevents[$ik] = $fields[2];

    }
    $cursor->finish();


    $sql="SELECT startTime FROM $JobStatusT where jobfileName like '$trigname%$prodTag%$basefile' ";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute();

    while( my $mstrm = $cursor->fetchrow() ) {
 
    $strtime[$ik] = $mstrm;
    }
    $cursor->finish();


 $nn = 0;
 @dbmufiles = ();
 @dbmusize = ();

    $sql="SELECT distinct filename, mudstsize FROM $ProdSizeT where  filename = '$mufiles[$ik]' and prodtag = '$prodTag' ";
    
    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute();
   
   while( @fields = $cursor->fetchrow() ) {
 
    $dbmufiles[$nn] = $fields[0];
    $dbmusize[$nn]  = $fields[1];
    $nn++;

    }
    $cursor->finish();

      if( !defined $dbmufiles[0] ) {

    print "File  ",$mufiles[$ik],"   not found in the table, insert file ","\n"; 

   $sql="insert into $ProdSizeT set Trigset = '$trigname', prodtag = '$prodTag', runnumber = '$runnum[$ik]', filename = '$mufiles[$ik]', mudstsize = '$musizes[$ik]', mudstEvents = '$muevents[$ik]', daqsize = '$daqsizes[$ik]', daqEvents = '$daqevents[$ik]', createtime = '$crtime[$ik]', starttime = '$strtime[$ik]' " ;    

      $dbh->do($sql) || die $dbh->errstr;

#      }elsif( $dbmusize[0] ne $musizes[$ik] ) {

#   $sql="update $ProdSizeT set  mudstsize = '$musizes[$ik]', mudstEvents = '$muevents[$ik]',  createtime = '$crtime[$ik]'  where filename = '$mufiles[$ik]' and prodtag = '$prodTag' " ;  

#      $dbh->do($sql) || die $dbh->errstr;

      }
 }
 

 &StDbDisconnect();

exit;


######################

sub StDbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
