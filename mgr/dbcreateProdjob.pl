#! /opt/star/bin/perl -w
#
#  
#   
#  
#
# L.Didenko
#
#  dbcreateProdjob.pl  script to create jobfiles for dst production
#
######################################################################

use Class::Struct;
use CGI;

require "/afs/rhic/star/packages/SL99i/mgr/dbDstProdSetup.pl";


my $debugOn=0;

my @prodPeriod = ("prod4", "prod5");

###Set directories to be created for jobfiles

my $DISK1        = "/star/rcf/disk00001/star/";
my $TOPHPSS_SINK = "/home/starsink/raw";
my $TOPHPSS_RECO = "/home/starreco/reco";
my $JOB_LOG;
my $JOB_DIR;  
my $chain_opt    =  "daq";
my $ii = 0;
my $nseries = scalar(@prodPeriod);

$ii = 0;

struct JfilAttr => {
          proSer  => '$',
         evtType  => '$',
         dir_set  => '$', 
       file_name  => '$',
          dfield  => '$',
  };

struct OptAttr => {
         prodSer  => '$',
          evType  => '$',  
          chaOpt  => '$',
        };
         
### connect to the DB

&StDbDstProdConnect();

my @jobf_set;
my @jobOpt;
my $jobf_no = 0;
my $njobOpt = 0;

# for ($ii=0; $ii<nseries; $i++) {

 $sql="SELECT prodSeries, eventType, chainOpt  FROM $ProdOptionsT where prodSeries = '$prodPeriod[$ii]' ";

   $cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute;
 
 while(@fields = $cursor->fetchrow) {
   my $cols=$cursor->{NUM_OF_FIELDS};
      $jObAdr = \(OptAttr->new());
 

  for($i=0;$i<$cols;$i++) {
   my $fvalue=$fields[$i];
     my $fname=$cursor->{NAME}->[$i];
    print "$fname = $fvalue\n" if $debugOn;

        ($$jObAdr)->prodSer($fvalue)   if( $fname eq 'prodSeries');
        ($$jObAdr)->evType($fvalue)    if( $fname eq 'eventType');
        ($$jObAdr)->chaOpt($fvalue)    if( $fname eq 'chainOpt');
  }

  $jobOpt[$njobOpt] = $jObAdr;
  $njobOpt++;
 }

# }
 
  $ii = 0;

# for ($ii=0; $ii<nseries; $i++) {

 $sql="SELECT prodSeries, eventType, DstDir, fileName, mfield  FROM $DstProductionT where prodSeries = '$prodPeriod[$ii]' AND disk_size = 0 AND jobfile = 'no' ";

 $cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;
 
 while(@fields = $cursor->fetchrow) {
   my $cols=$cursor->{NUM_OF_FIELDS};
      $fObjAdr = \(JfilAttr->new());
 

  for($i=0;$i<$cols;$i++) {
   my $fvalue=$fields[$i];
     my $fname=$cursor->{NAME}->[$i];
    print "$fname = $fvalue\n" if $debugOn;

        ($$fObjAdr)->proSer($fvalue)     if( $fname eq 'prodSeries');
        ($$fObjAdr)->evtType($fvalue)    if( $fname eq 'eventType');
        ($$fObjAdr)->dir_set($fvalue)    if( $fname eq 'DstDir');
        ($$fObjAdr)->file_name($fvalue)  if( $fname eq 'fileName');       
        ($$fObjAdr)->dfield($fvalue)     if( $fname eq 'mfield');
  }

  $jobf_set[$jobf_no] = $fObjAdr;
  $jobf_no++;
 }
#}


my $sjob;
my $flname;
my $jfile;
my $jobnm;
my $mSeries;
my $evt_type;
my $mchain;
my $mEvt;
my $prSeries;
my $optchain;
my $jchain;

foreach $jobnm (@jobf_set){
    $prSeries = ($$jobnm)->proSer;   
    $evt_type    = ($$jobnm)->evtType; 
    $sjob    = ($$jobnm)->dir_set;
    $flname  = ($$jobnm)->file_name;
    $daqfield =($$jobnm)->dfield;   
    $jfile   = $flname;
    $jfile =~ s/.daq//g;

      $JOB_LOG =  $DISK1 . $prSeries . "/log/daq";
      $JOB_DIR =  "/star/u2e/starreco/" . $prSeries . "/requests/daq/";         
    foreach $optchain (@jobOpt) {
        $mSeries  = ($$optchain)->prodSer;
        $mEvt     = ($$optchain)->evType;
        $mchain   = ($$optchain)->chaOpt;
         
        next if ( $prSeries ne $mSeries); 
        next if ( $evt_type ne $mEvt); 
        if ( ($daqfield eq 0) && $mchain =~ /FieldOff/)  {
             $jchain = $mchain ;
#       print "field, chain : ", $daqfield, $jchain, "\n";
       creat_jobs($jfile, $sjob, $jchain ); 
	   }
 
        elsif ( ($daqfield eq 50) && $mchain =~ /HalfField/)  {
             $jchain = $mchain ;
#      print "field, chain : ", $daqfield, $jchain, "\n";
     creat_jobs($jfile, $sjob, $jchain );
	   }
        elsif ( ($daqfield eq 100) && $mchain =~ /FieldOn/)  {
             $jchain = $mchain ;

#     print "field, chain : ", $daqfield, $jchain, "\n";
     creat_jobs($jfile, $sjob, $jchain );
      } 
}

  }

# finished with data base
 &StDbDstProdDisconnect();

exit();


### create jobfiles to get default set of output files

 sub creat_jobs($$$$) {

 my $gfile = $_[0];
 my $Jset  = $_[1]; 
 my $fchain = $_[2]; 
 my $job_set;
 my @parts;
 my $Jsetd;


 $job_set = $Jset;
  @parts =  split ("/", $Jset);
  $Jsetd = "daq/" . $parts[2] . "/" . $parts[3];

 $job_set =~ s/\//_/g;
 $fchain =~ s/_/,/g;

# print $job_set, "\n";
 
 my $jb_new = $JOB_DIR . "/new_jobs/" .  $job_set . "_" . $gfile;
 
      my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $Jsetd;
      my $hpss_raw_file = $gfile . ".daq";
      my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $Jset;
      my $hpss_dst_file0 = $gfile . ".event.root";
      my $hpss_dst_file1 = $gfile . ".dst.root";
      my $hpss_dst_file2 = $gfile . ".hist.root";
      my $hpss_dst_file3 = $gfile . ".dst.xdf";
      my $executable     = "/afs/rhic/star/packages/SL99g_7/mgr/bfc.csh";
      my $executableargs = $fchain;
      my $log_dir       = $JOB_LOG;
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


