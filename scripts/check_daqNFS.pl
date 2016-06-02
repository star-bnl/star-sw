#! /usr/local/bin/perl -w
#
# L. Didenko
#
# check_daqNFS.pl - script to check availability of daq files on NFS restored with DataCarousel
# and update status of daq files on DB table jobs_prod_2013. Create new list of files which failed
# to be restored by DataCarousel in first attempt.
#
###################################################################################################

use DBI;
use lib "/afs/rhic/star/packages/scripts";
use FileCatalog;
use Time::Local;

my $prodTg = $ARGV[0];

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="Embedding_job_stats";

my $JobStatusT = "jobs_prod_2016";

$fC1 = FileCatalog->new();
$fC1->connect_as("Admin");

my $nfspath = "/star/data16/GRID/daq/2015/";
my $nfile = 0;
my $dbfile = "none";
my $daqstat;
my $dsize = 0;
my $ndays = 2;
my $daydiff = 0;

my @dqfiles = ();
my @submtime = ();
my @prt = ();
my $sctime = 0;
my @fileset = ();
my $nlist = 0;
my @flname = ();
my $nll = 0;

  chdir $nfspath;

my @daqlist = `/bin/ls *.daq` ;

my ($sec,$min,$hour,$mday,$mon,$yr) = localtime();

 $mon++;
 if( $mon  < 10) { $mon  = '0'.$mon  };
 if( $mday < 10) { $mday = '0'.$mday };
 if( $hour < 10) { $hour = '0'.$hour };
 if( $min  < 10) { $min  = '0'.$min  };
 if( $sec  < 10) { $sec  = '0'.$sec  };

my $year = $yr + 1900;
my $timestamp = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;
my $today = $year.$mon.$mday ;
my $nowtime = $year.$mon.$mday."-".$hour."-".$min;


 &StDbConnect();

print "Size of array ", scalar(@daqlist), "\n";

if(scalar(@daqlist) > 1 ) {

 foreach my $daqfile (@daqlist) {
  chop $daqfile;

#  print $daqfile, "\n";

 ($dsize) = (stat($daqfile))[7];

#  print "Size of ", $daqfile,"  is  ",$dsize, "\n";

   $dbfile = "none";   
 
   $sql= "select inputFileName, inputFileExists from $JobStatusT where  prodTag = '$prodTg' and  inputFileName = '$daqfile' and inputFileExists = 'no' ";

   $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute();

      while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};

       for(my $i=0;$i<$cols;$i++) {

        $dbfile = $fields[0] ;
        $daqstat = $fields[1];
      }
    }
      $cursor->finish;
    
########

    next if ( !defined $dbfile) ;

    if ( $dbfile eq $daqfile and $dsize > 100000 ) {

   print "Update file status ", $daqfile,"  exists", "\n";
        
   $sql= "update $JobStatusT set inputFileExists = 'yes', inputFileSize = '$dsize' where inputFileName = '$daqfile' and  prodTag = '$prodTg' and inputFileExists = 'no' ";

   $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;
    
     }else{
    next;
    }
   }
 }

################## start creating list of files failed to be restorted on NFS


   $sql= "select inputFileName, carouselSubTime from $JobStatusT where  prodTag = '$prodTg' and inputFileExists = 'no' ";

   $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute();

      while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};

       for(my $i=0;$i<$cols;$i++) {

        $dqfiles[$nfile] = $fields[0] ;
        $submtime[$nfile] = $fields[1];
      }
      $nfile++;
    }
      $cursor->finish;

my $fname = "dcfile_resub"."_".$nowtime.".list";
my $DCfname = "/star/u/starreco/runkisti/".$fname;

my $dclog = "dcarousel_resub"."_".$nowtime.".log";
my $dcsubm = "/star/u/starreco/runkisti/".$dclog;


$fC1 = FileCatalog->new();
$fC1->connect_as("Admin");

 $nlist = 0;

for ($nlist=0; $nlist < $nfile; $nlist++) {

 my $dfile = $dqfiles[$nlist];
#  chop $dfile;
  print $dfile, "\n";

  @prt = ();

  @prt = split(" ",$submtime[$nlist]);  
  $sctime = $prt[0];
  $sctime =~ s/-//g;
  $daydiff = $today -  $sctime;

#  print "Check submission time ", $today,"   ",$sctime,"   ",$daydiff, "\n";

  if ( $daydiff >= $ndays and  $daydiff < 4 ) {

      $nll++;

 if (!open (CFILE, ">$DCfname")) {printf ("Unable to create file %s\n",$DCfname);}

  @fileset = ();

 $fC1->set_context("filename=$dfile","filetype=online_daq");

 @fileset = $fC1->run_query("path","filename");

 $fC1->clear_context();
 
 $flname[$nlist] = $fileset[0];
 $flname[$nlist] =~ s/::/\//g; 

 print CFILE $flname[$nlist], "\n";

 $sql= "update $JobStatusT set carouselSubTime = '$timestamp' where prodTag = '$prodTg' and inputFileName = '$dfile' ";
 $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

 }else{          #$daydiff
    next;
  }

############

 }  # for 

#  close (CFILE);

 $fC1->destroy();  

 &StDbDisconnect();

 if( $nll >= 1 ) {

  close (CFILE);

    `hpss_user.pl -a -r $nfspath -f $DCfname >& $dcsubm`;

  }

 exit ;

######################
sub StDbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}


######################
sub StDbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
