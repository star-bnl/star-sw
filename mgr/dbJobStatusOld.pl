#! /opt/star/bin/perl -w
#
#  
#
#  L.Didenko.
#
#  dbJobStatusOld.pl  script to fill in JobStatus table for files
#  from MDC1 through prod4
###############################################################################


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
 off_tdaq_tpc_HalfField_global_dst_event_analysis_tree_xout => 'tdaq1',
 off_tdaq_tpc_FieldOff_global_dst_event_analysis_tree_xout => 'tdaq2',
 off_tdaq_tpc_FieldOn_global_dst_event_analysis_tree_xout => 'tdaq2' 
    );        
                
my @jobIn_set;
my $jobIn_no = 0;

struct JFileAttr => {
          setN   => '$', 
         fileN   => '$',
        NjobId   => '$',
          NEvt   => '$', 
		    };

$sql="SELECT dataset, fName, Nevents FROM $FilesCatalogT WHERE fName LIKE '%fz' OR fName LIKE '%fzd' OR fName LIKE '%daq'";
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
       ($$fObjAdr)->NEvt($fvalue)    if( $fname eq 'Nevents'); 
}

  $jobIn_set[$jobIn_no] = $fObjAdr;
  $jobIn_no++;

}

my @jobOut_set;
my $jobOut_no = 0;

  $sql="SELECT jobID, dataset, fName, Nevents FROM $FilesCatalogT WHERE fName LIKE '%xdf'";
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
       ($$fObjAdr)->NEvt($fvalue)    if( $fname eq 'Nevents');                      
  }

      $jobOut_set[$jobOut_no] = $fObjAdr;
      $jobOut_no++; 


}

######### declare variables needed to fill the JobStatus table

my @parts;
my $mchainOp;
my $pr_chain;
my $summaryFile;
my $dsumDir;


my $mflName = "n\/a";
my $mjobID = "n\/a";
my $mprodSr = "n\/a";
my $mjobFname = "n\/a";
my $mjobFdir = "archive";
my $msumFile = "n\/a";
my $msumDir = "n\/a";
my $mjobSt = "done";
my $mNev  = 0;
my $mCPU = 0;
my $mmemSz = 0;
my $mNoTrk = 0;
my $mNoVert = 0;
my $mchName = "n\/a";
my $mnodeId = "rcrs";
my $startId = "Job_mdc1";
my $startSer = "mdc1";
my $new_id = 0;



### need to clear the jobRelationsT table first
 $sql="delete from $jobRelationsT";
 $cursor =$dbh->prepare($sql)
     || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;

### need to clear the JobStatusT table first
 $sql="delete from $JobStatusT";
 $cursor =$dbh->prepare($sql)
     || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;


###insert first line to JobStatusT table get last ID 

    $sql="insert into $JobStatusT set ";    
    $sql.="jobID='$startId',"; 
    $sql.="prodSeries='$startSer'";
     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
   $new_id = $dbh->{'mysql_insertid'};

 
### start loop over input files

 my $filename;

foreach my $jobnm (@jobIn_set){
    my $jset = ($$jobnm)->setN;
    my $flname = ($$jobnm)->fileN;
    my $jfile = $flname;
    if($jfile =~ /fzd/) {
     $jfile =~ s/.fzd//g;
   }
    elsif ($jfile =~ /fz/) {
     $jfile =~ s/.fz//g;
   }
   elsif ($jfile =~ /daq/) {
     $jfile =~ s/.daq//g;
   }   
### find corresponding output files
  
   foreach my $dsjob (@jobOut_set) {
      my $dsset   = ($$dsjob)->setN;
      my $dsfile  = ($$dsjob)->fileN;
      my $dsJobId = ($$dsjob)->NjobId;
      my $Nevts   = ($$dsjob)->NEvt; 
       next if ($dsJobId eq "n\/a" );
         if ($dsfile=~ /$jfile/) {
         @parts = split("_",$dsJobId); 
#         print "JobId = ", $dsJobId , "File name =  ", $dsfile, "\n"; 
         $mprodSr = $parts[1];
         $mflName = $flname; 
###  create Job ID

         $myID = 100000000 + $new_id;

### define values for JobStatus table
        
         $msumFile = $jfile . ".sum";
        $mjobFdir = "archive";
       if ( $parts[3] ne "complete" && $parts[3] ne "year2y" && $parts[3] ne "year1a" && $parts[3] ne "year2a" && $parts[3] ne "year2x" ) {
         $mchainOp = $parts[2] . "_". $parts[3] . "_" . $parts[4];                  
       } else {
         $mchainOp = $parts[2] . "_". $parts[3];
       }
          print "chain = " ,$mchainOp, "\n"; 
         $mchName = $chainHash{$mchainOp};
#         print "chainHash = " ,$chainHash{$mchainOp}, "\n";
         print "JobID = ", $myID, "chain = ", $mchName , "\n";
         $mjobID = "Job". $myID . "_" . $parts[1] ."_". $mchName  ;
 
         if( $mprodSr eq "mdc1" || $mprodSr eq "mdc2") {

        $mNev = $Nevts;
        $mCPU = 0;
        $mmemSz = 0;
        $mNoTrk = 0;
        $mNoVert = 0; 
        $mjobFname = "n\/a";
        $msumDir = "n\/a"; 
        $mjobSt = "done";
        $mnodeId = "rcrs"; 
  
       print "JOB ID = " ,$mjobID, "\n";

### fill JobStatus table for mdc1 and mdc2
      print "filling JobStatus table\n";
 
     &fillJSTable();   

### fill  jobRelations table
       print "filling jobRelations table\n";
      &fillJRelTable();

       }
      elsif ($mprodSr eq "postmdc2" || $mprodSr eq "prod4") { 
      $dsumDir = "/star/rcf/disk00001/star/" . $mprodSr . "/" . "sum/" . $parts[2];        
       opendir(DIR, $dsumDir) or die "can't open $dsumDir\n";
    while( defined($filename = readdir(DIR)) ) {
      next if $filename =~ /^\.\.?$/;
        next if ( !($filename =~ /.sum$/) );
       if ( $filename =~ /$msumFile/ ) {
         $msumDir = $dsumDir;
          $summaryFile = $msumDir ."/". $msumFile;
         print "Sum File = ", $summaryFile, "\n";
          &sumInfo("$summaryFile",1);
       print "Chain = ", $pr_chain, "\n";
       if( $pr_chain eq "tfs_y1b_-emc_eval_fzin_xout") {
          $mchName = "tfs2s";
       }
	 elsif ( $pr_chain eq "off_tdaq_tpc_HalfField_global_dst_event_analysis_tree_xout")  {
         $mchName = "tdaq1";
        }
	 elsif ( $pr_chain eq "off_tdaq_tpc_FieldOff_global_dst_event_analysis_tree_xout")  {
         $mchName = "tdaq2";
        }
	 elsif ( $pr_chain eq "off_tdaq_tpc_FieldOn_global_dst_event_analysis_tree_xout")  {
         $mchName = "tdaq3";
        } 
       else {
         $mchName = $chainHash{$mchainOp};
        }       
       $mjobID = "Job" . $myID . "_" . $parts[1] ."_". $mchName  ;
         print "JOB ID = " ,$mjobID, "\n";  

### fill JobStatus table for postmdc2 and prod4
       print "filling JobStatus table\n";
 
       &fillJSTable(); 
  
### fill  jobRelations table
       print "filling jobRelations table\n";
       &fillJRelTable();
         last;
      }  else {
         next;
       }
     }   
      closedir DIR;
  }
#    closedir DIR;

       }
     }
    }  

###delete from $JobStatusT inserted JobID

    $sql="delete from $JobStatusT WHERE ";    
    $sql.="jobID='$startId' AND "; 
    $sql.="prodSeries='$startSer'";
     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

# finished with data base
  &StDbProdDisconnect();

 exit;

################################################################################
sub fillJSTable {

    $sql="insert into $JobStatusT set ";
    $sql.="jobID='$mjobID',";
    $sql.="prodSeries='$mprodSr',";
    $sql.="jobfileName='$mjobFname',";
    $sql.="jobfileDir='$mjobFdir',";
    $sql.="sumFileName='$msumFile',";
    $sql.="sumFileDir='$msumDir',";
    $sql.="jobStatus='$mjobSt',";
    $sql.="NoEvents='$mNev',";
    $sql.="mem_size_MB='$mmemSz',";
    $sql.="CPU_per_evt_sec='$mCPU',";
    $sql.="avg_no_tracks='$mNoTrk',";
    $sql.="avg_no_vertex='$mNoVert',";
    $sql.="chainName='$mchName',";
    $sql.="nodeID='$mnodeId'";

   print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
    $new_id = $dbh->{'mysql_insertid'};     
  
  }

###############################################################################
sub fillJRelTable {

    $sql="insert into $jobRelationsT set ";
    $sql.="JobID='$mjobID',";
    $sql.="prodSeries='$mprodSr',";
    $sql.="inputFile='$mflName'"; 

   print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

   }

##############################################################################

sub sumInfo {

my ($jb_sum,$useless) = @_;   
my $sum_line ;

my @word_sum;

 
my @output = `more $jb_sum`; 
   foreach my $sum_line (@output) {
            chop $sum_line;

##get node name
             if ($sum_line =~ /Starting job execution/) {
              @word_sum = split (" ", $sum_line);
              $mnodeId = $word_sum[11];
	    }
##get job status
      if ($sum_line =~ /Segmentation violation/) {
              $mjobSt = "crash";
          }
     elsif ($sum_line =~ /buss error/) {
             $mjobSt = "bus_err";
        }  
     elsif ($sum_line =~ /Job status:/) {
        @word_sum = split (":", $sum_line);
         $mjobSt = $word_sum[1];
     } 

## get number of events done
   
   if ($sum_line =~ /Number of Events Done/ ) {
     @word_sum = split (":", $sum_line);          
       $mNev = $word_sum[1];
   } 
       
##get chain
         if( $sum_line =~ /QAInfo:Requested chain is/ ) {
           @word_sum = split (":", $sum_line); 
             $pr_chain = $word_sum[2];
             $pr_chain =~ s/ /_/g;
            if ( $pr_chain =~ /^\s*_/ ) {
              my $mIndex = index $pr_chain, "_";
                $pr_chain = substr( $pr_chain, $mIndex+1);
               } 
              
            }
##get max memory size during execution
            if ($sum_line =~ /Package   tree:/ ) {
              @word_sum = split (" ", $sum_line);
                $mmemSz = $word_sum[5];
            }
## get CPU per event
             next if ($sum_line =~ /Command string/);
            if($sum_line =~ /bfc/ ) {             
           @word_sum = split (" ", $sum_line);
#          print "CPU = ", $sum_line, "\n";   
            if($word_sum[7] =~ /Cpu/) { 
              $mCPU  = $word_sum[10];  
           }
          }
## get everage number of tracks in the event

      if($sum_line =~ /QAinfo: number of tracks/) {
       @word_sum = split (" ", $sum_line) ;  
              $mNoTrk = $word_sum[4];
     }   
## get everage number of vertex in the event

     if($sum_line =~ /QAinfo: number of vertices/) {     
         @word_sum = split (" ", $sum_line) ;
          $mNoVert = $word_sum[4]; 
     }
   }
}

