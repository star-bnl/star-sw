#! /opt/star/bin/perl -w
#
#  
#
#  
#  L.Didenko
#  dbupdateJobID_Old.pl  script to update FilesCatalog table with OLD data
#  from MDC1 through prod4 
#####################################################################################

use Mysql;
use Class::Struct;
use File::Basename;
use File::Find;

require "/afs/rhic/star/packages/DEV/mgr/dbProdSetup.pl";

my $debugOn=0;



###Set directories to be created for jobfiles
my $DISK1        = "/star/rcf/disk00001/star";

### connect to the DB
&StDbProdConnect();

my %chainHash = (
                     tfs_year_1b => 'tfs1b',
                     tfs_year1a  => 'tfs1a',
                     tfs_year2a  => 'tfs2a',
                     tss_year2a  => 'tss2a',
                     tfs_year2y  => 'tfs2y',
                     tfs_year2x  => 'tfs2x',
                     tfs_year_2a => 'tfs2a',
                     tss_year_1b => 'tss1b',
                     tss_year_2a => 'tss2a',
                     trs_year_1b => 'trs1b',
                     tfs_year_1s => 'tfs1s',
                     tss_year_1s => 'tss1s',
                     tfs_year_1a => 'tfs1a',
                     tss_year_1a => 'tss1a',
                     tfs_year_2b => 'tfs2b',
                     tss_year_2b => 'tss2b',
                    tfs_complete => 'tfs6c',
       tfs_y1b_emc_eval_fzin_xout=> 'tfs2s',
          tfs_y1b_eval_fzin_xout => 'tfs4b',
 tfs_y1b_eval_allevent_fzin_xout => 'tfs5b',
 off_tdaq_HalfField_tpc_global_dst_event_analysis_tree_xout => 'tdaq1',
 off_tdaq_FieldOff_tpc_global_dst_event_analysis_tree_xout => 'tdaq2',
 off_tdaq_FieldOn_tpc_global_dst_event_analysis_tree_xout => 'tdaq2' 
    );        
                
my @jobIn_set;
my $jobIn_no = 0;

struct JFileAttr => {
          setN   => '$', 
         fileN   => '$',
        NjobId   => '$',
          comp   => '$',
          form   => '$',
		    };

 struct JStAttr  => {
          prodSr => '$',
          Nmfile => '$',         
         NojobId => '$',
          NoEvts => '$',
		    };

my @jobOut_set;
my $jobOut_no = 0;

my $mJobId = "n\/a";
my $mNev  = 0;


  $sql="SELECT jobID, dataset, fName, component, format FROM $FilesCatalogT WHERE fName LIKE '%xdf' OR fName LIKE '%root'";
  $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
       $fObjAdr = \(JFileAttr->new());
 

   for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
      my $fname=$cursor->{NAME}->[$i];
       print "$fname = $fvalue\n" if $debugOn;

       ($$fObjAdr)->setN($fvalue)    if( $fname eq 'dataset');
       ($$fObjAdr)->fileN($fvalue)   if( $fname eq 'fName'); 
       ($$fObjAdr)->NjobId($fvalue)  if( $fname eq 'jobID');
       ($$fObjAdr)->comp($fvalue)    if( $fname eq 'component');
       ($$fObjAdr)->form($fvalue)    if( $fname eq 'format');                     
  }

      $jobOut_set[$jobOut_no] = $fObjAdr;
      $jobOut_no++; 


}

my $extn;
my $basename;
my @parts;
my $mprodSr;
my $sumFile;
my $mchOpt;
my $fileName;
my $OldJobId; 
my $mNevHi;

### start loop over reco files

      foreach my $dsjob (@jobOut_set) {
      my $dsset   = ($$dsjob)->setN;
      my $dsfile  = ($$dsjob)->fileN;
      my $dsJobId = ($$dsjob)->NjobId;
      my $dscomp  = ($$dsjob)->comp;
      my $dsform  = ($$dsjob)->form;
      next if ($dsJobId eq "n\/a" );
        my $jfile = $dsfile;
     if($jfile =~ /_h_dst.xdf/) {
    $jfile =~ s/_h_dst.xdf//g;
    }
     elsif($jfile =~ /.p1.xdf/) {
    $jfile =~ s/.p1.xdf//g;
    }
     elsif($jfile =~ /_p1.xdf/) {
    $jfile =~ s/_p1.xdf//g;
    }
     elsif($jfile =~ /.p2.xdf/) {
    $jfile =~ s/.p2.xdf//g;
    }
     elsif($jfile =~ /_dst.xdf/) {
     $jfile =~ s/_dst.xdf//g;
   }
    elsif($jfile =~ /.dst.xdf/) {
     $jfile =~ s/.dst.xdf//g;
   }
    elsif($jfile =~ /evts.xdf/) {
     $jfile =~ s/.xdf//g;
   }
     elsif($jfile =~ /.dst.root/) {
     $jfile =~ s/.dst.root//g;
   }
     elsif($jfile =~ /_dst.root/) {
     $jfile =~ s/_dst.root//g;
   }
     elsif($jfile =~ /evts.root/) {
     $jfile =~ s/.root//g;
   }
      elsif($jfile =~ /_hard3.xdf/) {
    $jfile =~ s/_hard3.xdf//g;
    }
     else {
     $extn = "." . $dscomp . "." .$dsform;
     $basename = basename("$dsfile","$extn");  
     $jfile = $basename;
   }
    print "File name = ", $dsfile, "  Basename = ", $jfile, "  JOB ID = ", $dsJobId,  "\n";
     $sumFile = $jfile . ".sum";
     $fileName = $dsfile;
     $OldJobId = $dsJobId;


   my @parts = split ("_",$dsJobId);

   $mprodSr = $parts[1];
    $mchOpt = $parts[2]; 
     print "prodSeries = ",$mprodSr, "  chainOpt = ",$mchOpt, "\n", " SumFile = ",$sumFile, "\n" ;   
 
  $sql="SELECT JobID, prodSeries, sumFileName, NoEvents FROM $JobStatusT WHERE JobID LIKE '%$mchOpt%' AND prodSeries = '$mprodSr' AND sumFileName = '$sumFile'";
  $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
       $fObjAdr = \(JStAttr->new());
 

   for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
      my $fname=$cursor->{NAME}->[$i];
       print "$fname = $fvalue\n" if $debugOn;

       ($$fObjAdr)->prodSr($fvalue)   if( $fname eq 'prodSeries');
       ($$fObjAdr)->Nmfile($fvalue)   if( $fname eq 'sumFileName'); 
       ($$fObjAdr)->NojobId($fvalue)  if( $fname eq 'JobID');
       ($$fObjAdr)->NoEvts($fvalue)   if( $fname eq 'NoEvents');                     
  }

#      $jobSt_set[$jobSt_no] = $fObjAdr;
#      $jobSt_no++; 

       $mjobId = ($$fObjAdr)->NojobId ;
       $mNev = ($$fObjAdr)->NoEvts;
       $mNevHi = $mNev;
       print "JobId = ", $mjobId, "File name = ", $fileName, "Old JobID = ",$OldJobId, "\n";
       print "updating FileCatalogT table\n";
       
      &updateFCTable();
   }

  }

# finished with data base
  &StDbProdDisconnect();

 exit;

################################################################################
sub updateFCTable {

    $sql="update $FilesCatalogT set ";
    $sql.="jobID='$mjobId',";
    $sql.="Nevents='$mNev',";
    $sql.="NevHi='$mNevHi'";  
    $sql.="WHERE fName = '$fileName' AND jobID = '$OldJobId'";  
   print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
  
  }

###############################################################################


