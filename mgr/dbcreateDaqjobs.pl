#! /opt/star/bin/perl -w
#
#  
#   
#  
#
#  dbcreateDaqjobs.pl  script to create jobfiles to proccess daq files
#
######################################################################

use Class::Struct;
use CGI;

require "/afs/rhic/star/packages/SL99h/mgr/dbDaqOperaSetup.pl";

my $debugOn=0;


###Set directories to be created for jobfiles
my $DISK1        = "/star/rcf/disk00001";
my $TOPHPSS_SINK =  "/home/starsink/raw";
my $TOPHPSS_RECO =  "/home/starreco/reco";
my $JOB_LOG      =  $DISK1 . "/star/prod4/log";
my $JOB_DIR      =  "/star/u2e/starreco/prod4/requests/"; 
my $chain_opt    =  "daq";

### connect to the DB
&StDbDaqOperaConnect();


my @jobf_set;
my $jobf_no = 0;



struct JFileAttr => {

         dir_set   => '$', 
       file_name   => '$',
         dfield    => '$',
		    };


 $sql="SELECT DstDir, fileName, mfield  FROM $DaqOperationT where disk_size = 0 AND jobfile = 'no' ";
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

        ($$fObjAdr)->dir_set($fvalue)     if( $fname eq 'DstDir');
        ($$fObjAdr)->file_name($fvalue)  if( $fname eq 'fileName');       
        ($$fObjAdr)->dfield($fvalue)     if( $fname eq 'mfield');
   }

  $jobf_set[$jobf_no] = $fObjAdr;
  $jobf_no++;


}
my $sjob;
my $flname;
my $jfile;
my $jobnm;


foreach $jobnm (@jobf_set){
    $sjob = ($$jobnm)->dir_set;
    $flname = ($$jobnm)->file_name;
    $daqfield =($$jobnm)->dfield;   
    $jfile = $flname;
     $jfile =~ s/.daq//g;

#    print $daqfield, "\n";
     creat_jobs($jfile, $sjob, $chain_opt,$daqfield );
 }

# finished with data base
&StDbDaqOperaDisconnect();

exit();


### create jobfiles to get default set of output files
 sub creat_jobs($$$$) {

 my $gfile = $_[0];
 my $Jset  = $_[1]; 
 my $process = $_[2];
 my $ffield = $_[3]; 
 my $job_set;
 my $executableargs;

 $job_set = $Jset;
 my $Jsetd = $Jset;
 $Jsetd =~ s/dst\/prod4/daq/g;  
 $job_set =~ s/\//_/g;

# print $job_set, "\n";
 
 my $jb_new = $JOB_DIR . $process . "/new_jobs/" .  $job_set . "_" . $gfile;
 
       my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $Jsetd;
       my $hpss_raw_file = $gfile . ".daq";
       my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $Jset;
       my $hpss_dst_file0 = $gfile . ".event.root";
       my $hpss_dst_file1 = $gfile . ".dst.root";
       my $hpss_dst_file2 = $gfile . ".hist.root";
       my $hpss_dst_file3 = $gfile . ".dst.xdf";
       my $executable     = "/afs/rhic/star/packages/SL99g/mgr/bfc.csh";
  if ($ffield eq 0) {
       $executableargs  = "off,tdaq,tpc,FieldOff,global,dst,event,analysis,tree,xout";
     }
 elsif($ffield eq 50) {
      $executableargs  = "off,tdaq,tpc,HalfField,global,dst,event,analysis,tree,xout";
     }
 elsif($ffield eq 100) {
      $executableargs  = "off,tdaq,tpc,FieldOn,global,dst,event,analysis,tree,xout";
     }

       my $log_dir       = $JOB_LOG . "/" . $process;
       my $log_name      = $gfile . ".log";
       my $err_log       = $gfile . ".err";
       if (!open (TOM_SCRIPT,">$jb_new")) {printf ("Unable to create job submission script %s\n",$jb_new);}
       print TOM_SCRIPT "mergefactor=1\n";
       print TOM_SCRIPT "#input\n";
       print TOM_SCRIPT "      inputnumstreams=1\n";
       print TOM_SCRIPT "      inputstreamtype[0]=HPSS\n";
       print TOM_SCRIPT "      inputdir[0]=$hpss_raw_dir\n";
       print TOM_SCRIPT "      inputfile[0]=$hpss_raw_file\n";
       print TOM_SCRIPT "#output\n";
       print TOM_SCRIPT "      outputnumstreams=4\n";
       print TOM_SCRIPT "#output stream \n";
       print TOM_SCRIPT "      outputstreamtype[0]=HPSS\n";
       print TOM_SCRIPT "      outputdir[0]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[0]=$hpss_dst_file0\n";
       print TOM_SCRIPT "      outputstreamtype[1]= HPSS\n";
       print TOM_SCRIPT "      outputdir[1]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[1]=$hpss_dst_file1\n";
       print TOM_SCRIPT "      outputstreamtype[2]=HPSS\n";
       print TOM_SCRIPT "      outputdir[2]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[2]=$hpss_dst_file2\n";
       print TOM_SCRIPT "      outputstreamtype[3]=HPSS\n";
       print TOM_SCRIPT "      outputdir[3]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[3]=$hpss_dst_file3\n";
       print TOM_SCRIPT "#standard out -- Should be five outputs\n";
       print TOM_SCRIPT "      stdoutdir=$log_dir\n";
       print TOM_SCRIPT "      stdout=$log_name\n";
       print TOM_SCRIPT "#standard error -- Should be five\n";
       print TOM_SCRIPT "      stderrdir=$log_dir\n";
       print TOM_SCRIPT "      stderr=$err_log\n";
       print TOM_SCRIPT "      notify=starreco\@rcf.rhic.bnl.gov\n";
       print TOM_SCRIPT "#program to run\n";
       print TOM_SCRIPT "      executable=$executable\n";
       print TOM_SCRIPT "      executableargs=$executableargs\n";
       close(TOM_SCRIPT);

}


# finished with data base
&StDbDaqOperaDisconnect();
