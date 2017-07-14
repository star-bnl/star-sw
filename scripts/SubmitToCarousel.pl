#! /usr/local/bin/perl -w
#
# L. Didenko
#
#
# SubmitToCarousel.pl - scripts automatically generate and  submit to DataCarousel list of N daq files 
# using query from FileCatalog and list of runnumbers to be processed in production.
# List of daq files is created if number of files in the designed directory less than MAXNUM. 
#
##############################################################################################################

use lib "/afs/rhic/star/packages/scripts";
use FileCatalog;
use DBI;
use Time::Local;

my $prodSer = $ARGV[0]; 
my $fileName = $ARGV[1];  

my $lockfile = "/star/u/starreco/runkisti/submission.lock";

$fC1 = FileCatalog->new();
$fC1->connect_as("Admin");


$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="Embedding_job_stats";

my $JobStatusT = "jobs_prod_2017";

my $nfspath = "/star/data11/GRID/daq/2016/";
my $daqpat = $nfspath."*.daq";
my @prdset = ();
my $nl = 0;

my @daqlist = `ls $daqpat` ;

my $MAXNUM = 1000;
my $LIMNUM = 600;
#my $MAXNUM = 500;
#my $LIMNUM = 300;

#my $NNUM = 100;
my $NNUM = 300;
my $SUMNUM = scalar(@daqlist);

print "There are  ", scalar(@daqlist),"  daq files in the ", $nfspath,"  directory", "\n";
 
 &StDbConnect();

 $sql= "select inputFileName from $JobStatusT where  prodTag = '$prodSer' and inputFileExists = 'no' ";

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

       while( $mfile = $cursor->fetchrow() ) {
          $prdset[$nl] = $mfile;
          $nl++;
       }
    $cursor->finish();


 if(scalar(@daqlist) <= $LIMNUM and scalar(@prdset) < $NNUM  ) {

 if( -f $lockfile) {
     `/bin/rm  $lockfile` ;

#######

my $listName = "/star/u/starreco/".$fileName;
my @runSet = ();

  open (RUNLIST, $listName ) or die "cannot open $listName: $!\n";

  @runSet = <RUNLIST>;

  close(RUNLIST);

my $timestamp ;
my $lastid = 0;
my $prodrun = 0;
my $nfiles = 0;
my $nruns = 0;
my @fileset = ();
my @filelist = ();
my @trigname = ();
my @daqfile = ();
my @daqevents = ();
my $nlist = 0;
my @prt = ();
my $myrun;
my $todate;

 $lastid = scalar(@runSet) -1;

 print "There are ",$lastid," run numbers in the list", "\n";

 my $nextName = "/star/u/starreco/runkisti/".$lastid."_".$fileName;

#######

my ($sec,$min,$hour,$mday,$mon,$yr) = localtime();

 $mon++;
 if( $mon  < 10) { $mon  = '0'.$mon  };
 if( $mday < 10) { $mday = '0'.$mday };
 if( $hour < 10) { $hour = '0'.$hour };
 if( $min  < 10) { $min  = '0'.$min  };
 if( $sec  < 10) { $sec  = '0'.$sec  };
	 
 $year = $yr + 1900;
 $timestamp = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;
 $todate = $year.$mon.$mday."-".$hour."-".$min;

 print "Today is  ",$timestamp, "\n";

my $fname = "dcfile"."_".$todate.".list";
my $DCfname = "/star/u/starreco/runkisti/".$fname;

my $dclog = "dcarousel"."_".$todate.".log";
my $dcsubm = "/star/u/starreco/runkisti/".$dclog;

 print "Input/log file name for DataCarousel  ", $DCfname,"   ", $dcsubm,"\n";

#####
     
 if ($lastid > 0 ) {

 `/bin/mv $listName $nextName`;

 if (!open (NEWLIST, ">$listName" ))  {printf ("Unable to create file %s\n",$listName);}
	 
 if (!open (CFILE, ">$DCfname")) {printf ("Unable to create file %s\n",$DCfname);};

########

 foreach $prodrun (@runSet) {
  chop $prodrun ;

  if($prodrun != 10000000 ) {

 print  $prodrun, "\n";

  @fileset = ();

# $fC1->set_context("runnumber=$prodrun","filetype=online_daq","filename~st_physics");

 $fC1->set_context("runnumber=$prodrun","filetype=online_daq","filename~st_upc","sanity=1","limit=0");

 @fileset = $fC1->run_query("trgsetupname","path","filename","events");

 $fC1->clear_context();

########

 $SUMNUM = $SUMNUM + scalar(@fileset);

 if($SUMNUM <= $MAXNUM ) {

 foreach my $line (@fileset) {

 @prt = ();
 @prt = split("::", $line);

 $trigname[$nlist] = $prt[0];
 $filelist[$nlist] = $prt[1]."/".$prt[2];
 $daqfile[$nlist] = $prt[2];
 $daqevents[$nlist] = $prt[3];

 print CFILE $filelist[$nlist], "\n";

# print  $prodrun,"   ",$filelist[$nlist],"  ", $nlist,"\n";

 $sql= "insert into $JobStatusT set datasetName = '$trigname[$nlist]', prodTag = '$prodSer', inputFileName = '$daqfile[$nlist]', inputFileEvents = '$daqevents[$nlist]', carouselSubTime = '$timestamp', inputFileExists = 'no' ";

 $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;   

 $nlist++;

  }

  $nruns++;

 }else{

     print "Number of suggested files > $MAXNUM ","\n";
 goto GO_END;    
 }
#####

#  if($nlist >= $MAXNUM ) {

 if($nlist >= 300 ) {

  close (CFILE);
  goto GO_SUBMIT; 
      }

    }else{
	 if (!open (NEWLIST, ">$listName" )){
	     printf ("Unable to create file %s\n",$listName);
	 }else{
		 print  "No more run numbers in the list", "\n";
		 print NEWLIST "10000000\n";
	close (NEWLIST);
	 }
     }
   }
 }


GO_SUBMIT:

 &StDbDisconnect();

    `hpss_user.pl -r $nfspath -f $DCfname >& $dcsubm`;

	 if (!open (NEWLIST, ">$listName" )){
	     printf ("Unable to create file %s\n",$listName);
	 } else {


	     if ($nruns <= $lastid ) {

		 for ( my $kk = $nruns; $kk <= $lastid; $kk++) {
		     $myrun = $runSet[$kk];
		     chop $myrun; 
#                 print "Before writing  ",$nruns,"   ",$kk,"   ",$lastid,"   ",$myrun, "\n"; 

		     print NEWLIST  "$myrun\n";  
		 }
                    close (NEWLIST);
	     } else {

		 print  "No more run numbers in the list", "\n";
		 print NEWLIST "10000000\n";
	     close (NEWLIST);
	     } 
	 }

#########

GO_END:  

     if (!open (SUBFILE, ">$lockfile" ))  {printf ("Unable to create file %s\n",$lockfile);}
 
     print SUBFILE "Submission done", "\n";

     close (SUBFILE);

  } else {
     exit;
 }

 $fC1->destroy();

}

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
