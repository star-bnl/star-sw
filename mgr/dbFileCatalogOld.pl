#! /opt/star/bin/perl -w
#
#  
#
# 
#
# L.Didenko
#
######################################################################
#
# dbFileCatalogOld.pl
#
#
# Create production database for all files from MDC1 through prod4 series
#
#
#

use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;


require "/afs/rhic/star/packages/DEV/mgr/dbProdSetup.pl";

struct FileAttr => {

    filename  => '$',
    dset      => '$',
    fpath     => '$', 
    fchain    => '$',
    dsize     => '$',
    timeS     => '$',
    faccess   => '$',
    fowner    => '$',
};

my $debugOn = 0;

 my @fileExt = (".root", ".xdf", ".fz", ".fzd");

my @SetG = (
               "auau100/venus412/default/b0_3/year_1b/hadronic_on",
               "auau100/venus412/default/b0_3/year_1s/hadronic_on",
               "auau100/venus412/default/b3_6/year_1s/hadronic_on",
               "auau100/venus412/default/b6_9/year_1s/hadronic_on",
               "auau200/hbt_ven/1d/r10/complete/hadronic_on",
               "auau200/hbt_ven/1d/r7/complete/hadronic_on",
               "auau200/hbt_ven/1d/r7/year_1b/hadronic_on",
               "auau200/hbt_ven/sol/r5/year_1b/hadronic_on",
               "auau200/hbt_ven/sol/r563/year_1b/hadronic_on",
               "auau200/hbt_ven/sol/r567/complete/hadronic_on",
               "auau200/hbt_ven/sol/r674/year_1b/hadronic_on",
               "auau200/hbt_ven/sol/r8910/complete/hadronic_on",
               "auau200/hbt_ven/ykp/r533/year_1b/hadronic_on",
               "auau200/hbt_ven/ykp/r571/complete/hadronic_on",
               "auau200/hbt_ven/ykp/r644/year_1b/hadronic_on",
               "auau200/hbt_ven/ykp/r8102/complete/hadronic_on",
               "auau200/hbt_vni/1d/r10/complete/hadronic_on",
               "auau200/hbt_vni/1d/r5/year_1b/hadronic_on",
               "auau200/hbt_vni/1d/r7/complete/hadronic_on",
               "auau200/hbt_vni/1d/r7/year_1b/hadronic_on",
               "auau200/hbt_vni/sol/r533/year_1b/hadronic_on",
               "auau200/hbt_vni/sol/r563/year_1b/hadronic_on",
               "auau200/hbt_vni/sol/r567/complete/hadronic_on",
               "auau200/hbt_vni/sol/r644/year_1b/hadronic_on",
               "auau200/hbt_vni/sol/r674/year_1b/hadronic_on",
               "auau200/hbt_vni/sol/r8910/complete/hadronic_on",
               "auau200/hbt_vni/ykp/r571/complete/hadronic_on",
               "auau200/hbt_vni/ykp/r8102/complete/hadronic_on",
               "auau200/hijet/default/central/year_1b/hadronic_on",
               "auau200/hijing/b0_3/jet05/year_1b/hadronic_on",
               "auau200/hijing/b0_3/jet10/year_1b/hadronic_on",
               "auau200/hijing/b0_3/jet15/year_1b/hadronic_on",
               "auau200/hijing/b0_3/jet20/year_1b/hadronic_on",
               "auau200/hijing/default/jet05/year_2a/hadronic_on",
               "auau200/hijing/default/jet10/year_2a/hadronic_on",
               "auau200/hijing/default/jet15/year_2a/hadronic_on",
               "auau200/hijing/default/jet20/year_2a/hadronic_on",
               "auau200/hijing135/Bjets/b0_3/year_2a/hadronic_on",
               "auau200/hijing135/Cjets/b0_3/year_2a/hadronic_on",
               "auau200/hijing135/default/b0_2/year1a/hadronic_off",
               "auau200/hijing135/default/b0_2/year1a/hadronic_on",
               "auau200/hijing135/default/b0_2/year2a/hadronic_off",
               "auau200/hijing135/default/b0_2/year2a/hadronic_on",
               "auau200/hijing135/default/b0_2/year2y/hadronic_on",
               "auau200/hijing135/default/b0_20/year2a/hadronic_on",
               "auau200/hijing135/default/b0_20/year2x/hadronic_on",
               "auau200/hijing135/default/b0_20/year_1b/hadronic_off",
               "auau200/hijing135/default/b0_20/year_2b/hadronic_on",
               "auau200/hijing135/default/b0_3/year1a/hadronic_off",
               "auau200/hijing135/default/b0_3/year1a/hadronic_on",
               "auau200/hijing135/default/b0_3/year2a/hadronic_on",
               "auau200/hijing135/default/b0_3/year2x/hadronic_on",
               "auau200/hijing135/default/b0_3/year_1b/hadronic_off",
               "auau200/hijing135/default/b0_3/year_1b/hadronic_on",
               "auau200/hijing135/default/b0_3/year_1h/hadronic_on",
               "auau200/hijing135/default/b0_9/year_2a/hadronic_on",
               "auau200/hijing135/default/b3_6/year_1h/hadronic_on",
               "auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on",
               "auau200/hijing135/jetq_off/b0_3/year_1c/hadronic_on",
               "auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on",
               "auau200/hijing135/jetq_on/b0_3/year_1c/hadronic_on",
               "auau200/hijing135/jetq_on/b0_3/year_1s/hadronic_on",
               "auau200/hijing135/jetq_on/b0_9/year_1b/hadronic_on",
               "auau200/hijing135/jetq_on/b3_6/year_1b/hadronic_on",
               "auau200/hijing135/jetq_on/b3_6/year_1s/hadronic_on",
               "auau200/hijing135/jetq_on/b6_9/year_1b/hadronic_on",
               "auau200/hijing135/jetq_on/b6_9/year_1s/hadronic_on",
               "auau200/hijing135/jetq_on/b9_12/year_1b/hadronic_on",
               "auau200/two_photon/default/b0_20/complete/hadronic_on",
               "auau200/two_photon/dtunuc/none/year_1a/hadronic_on",
               "auau200/two_photon/starlight/phi/year_1b/hadronic_on",
               "auau200/two_photon/starlight/rho/year_1b/hadronic_on",
               "auau200/two_photon/starlight/twogam/year_1b/hadronic_on",
               "auau200/venus412/default/b0_3/year_1b/hadronic_on",
               "auau200/venus412/default/b0_3/year_1s/hadronic_on",
               "auau200/venus412/default/b0_3/year_2a/hadronic_on",
               "auau200/venus412/default/b0_9/year_1b/hadronic_on",
               "auau200/venus412/default/b3_6/year_1b/hadronic_on",
               "auau200/venus412/default/b3_6/year_2a/hadronic_on",
               "auau200/venus412/default/b6_9/year_1b/hadronic_on",
               "auau200/venus412/default/b6_9/year_2a/hadronic_on",
               "auau200/venus412/default/b9_12/year_1b/hadronic_on",
               "auau200/venus412/default/b9_12/year_2a/hadronic_on",
               "auau200/venus412/halffield/b0_3/year_1b/hadronic_on",
               "auau200/vni/after/central/year_1b/hadronic_on",
               "auau200/vni/after/minb/year_1b/hadronic_on",
               "auau200/vni/before/central/year_1b/hadronic_on",
               "auau200/vni/before/minb/year_1b/hadronic_on",
               "augas100/venus412/hydrogen/b0_10/year_1b/hadronic_on",
               "augas100/venus412/nitrogen/b0_10/year_1b/hadronic_on",
               "cosmics/muon/default/none/year_1b/hadronic_on",
               "pau200/hijing/default/minbias/year_2a/hadronic_on",
               "pp200/pythia/compton/ptcut8/year_2a/hadronic_on",
               "pp200/pythia/default/ckin15/complete/hadronic_on",
               "pp200/pythia/default/ckin2/complete/hadronic_on",
               "pp200/pythia/default/ckin3/complete/hadronic_on",
               "pp200/pythia/default/ckin5/complete/hadronic_on",
               "pp200/pythia/default/jet15/year_1b/hadronic_on",
               "pp200/pythia/default/jet15/year_2a/hadronic_on",
               "pp200/pythia/default/minb/complete/hadronic_on",
               "pp200/pythia/default/minbias/year_1b/hadronic_on",
               "pp200/pythia/default/minbias/year_2a/hadronic_on",
               "sisi200/venus412/default/b0_3/year_1b/hadronic_on",
               "sisi200/venus412/default/b0_3/year_1s/hadronic_on",
           );

my @SetD = (
             "daq/1999/06",
             "daq/1999/07",
             "daq/1999/08",
             "daq/1999/12"
            );  

my @prodPeriod = ("mdc1", "mdc2", "postmdc2", "prod4");
my @recoDir = ("tfs_dst", "tss_dst", "tfs","tss","trs","tfsr","tssr","tfs_4");

my $disk0Reco    =  "/star/rcf/disk00000/star";
my $disk1Reco    =  "/star/rcf/disk00001/star";
my $topHpssSink  =  "/home/starsink/raw";
my $topHpssReco  =  "/home/starreco/reco";
my %prodHash = (
                 tfs_dst => 'mdc1',
                 tss_dst => 'mdc1',
                 tfs     => 'mdc2',
                 tss     => 'mdc2', 
                 trs     => 'mdc2',
                 tfsr    => 'postmdc2',
                 tssr    => 'postmdc2',
                 tfs_4   => 'prod4'
               );                       

my @diskRecoFiles;
my @hpssRawFiles;
my @hpssRecoFiles;

my %monthHash = (
		 "Jan" => 1,
		 "Feb" => 2, 
		 "Mar" => 3, 
		 "Apr" => 4, 
		 "May" => 5, 
		 "Jun" => 6, 
		 "Jul" => 7, 
		 "Aug" => 8, 
		 "Sep" => 9, 
		 "Oct" => 10, 
		 "Nov" => 11, 
		 "Dec" => 12
		 );



my $eachHpssFile;
my $eachRecoFile;
my $mrun;

########## Find Geant input files in HPSS

my @hpssGeantDirs;
my @hpssGeantFiles;

my $nHpssDirs = scalar(@SetG);
my $nHpssFiles = 0;

for( $ll = 0; $ll<scalar(@SetG); $ll++) {
  $hpssGeantDirs[$ll] = $topHpssSink . "/" . $SetG[$ll] . "/" . "gstardata";
  print "hpssGeantDir: $hpssGeantDirs[$ll]\n";
}

my $ftpSimu = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpSimu->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding Geant files in HPSS\n"; 
&walkHpss( $ftpSimu, \@hpssGeantDirs, \@hpssGeantFiles );
print "Total files: ".@hpssGeantFiles."\n";
 $ftpSimu->quit();


########## Find Daq input files in HPSS

my @hpssDaqDirs;
my @hpssDaqFiles;

my $nHpssDaqDirs = scalar(@SetD);
my $nHpssDaqFiles = 0;

for( $ll = 0; $ll<scalar(@SetD); $ll++) {
  $hpssDaqDirs[$ll] = $topHpssSink . "/" . $SetD[$ll];
  print "hpssDaqDir: $hpssDaqDirs[$ll]\n";
}

my $ftpDaq = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpDaq->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding Daq files in HPSS\n"; 
&walkDHpss( $ftpDaq, \@hpssDaqDirs, \@hpssDaqFiles );
print "Total files: ".@hpssDaqFiles."\n";
 $ftpDaq->quit();


########## Find reco files in HPSS

$nHpssFiles = 0;

my @hpssRecoDirs;
my $kk = 0;
my $kl = 0;

for( $ll = 0; $ll<scalar(@SetG); $ll++) {
  for ($kl = 0; $kl<scalar(@recoDir); $kl++) { 
  $hpssRecoDirs[$kk] = $topHpssReco ."/" .$SetG[$ll]. "/" . $recoDir[$kl];
    $kk++;
  print "hpssRecoDir: $hpssRecoDirs[$ll]\n" if $debugOn;
 }
}

$nHpssDirs = scalar(@hpssRecoDirs);

$ftpReco = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>200)
  or die "HPSS access failed";
$ftpReco->login("starreco","MockData") or die "HPSS access failed";

print "\nFinding reco files in HPSS\n"; 
 &walkHpss($ftpReco, \@hpssRecoDirs, \@hpssRecoFiles );
    print "Total files: ".@hpssRecoFiles."\n";
 $ftpReco->quit();


########## Find DST for online input files in HPSS

my  @ssdaq;
my @hpssDstDirs;
my @hpssDstFiles;
my $setDst;
 $nHpssDaqDirs = scalar(@SetD);
 $nHpssDaqFiles = 0;

for( $ll = 0; $ll<scalar(@SetD); $ll++) {
  @ssdaq = split("/", $SetD[$ll]);
  $setDst = "dst/" . $prodPeriod[3] . "/" . $ssdaq[1] . "/". $ssdaq[2];
  $hpssDstDirs[$ll] = $topHpssReco . "/" . $setDst;
  print "hpssDstDir: $hpssDstDirs[$ll]\n";
}

my $ftpRDaq = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpRDaq->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding daq DST files in HPSS\n"; 
&walkDHpss( $ftpRDaq, \@hpssDstDirs, \@hpssDstFiles );
print "Total files: ".@hpssDstFiles."\n";
 $ftpRDaq->quit();


######### declare variables needed to fill the database table
## for database filling

my $mJobId = "n\/a"; 
my $mrunId = 0;
my $mfileSeq = 0;
my $mevtType = 0;
my $mfName = "n\/a";
my $mpath  = "n\/a";
my $mdataSet = "n\/a";
my $msize = 0;
my $mcTime = 00-00-00;
my $mNevts = 0;
my $mNevtLo = 0;
my $mNevtHi = 0;
my $mowner = "n\/a";
my $mprotc = 0;
my $mtype = "n\/a";
my $mcomp = "n\/a";
my $mformat = "n\/a";
my $msite = "n\/a";
my $mhpss = "Y";
my $mstatus = 0;


 &StDbProdConnect();

# need to clear the FileCatalog table first here
 $sql="delete from $FilesCatalogT";
 $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;

## fill in tables with GEANT files 

   foreach $eachGeantFile (@hpssGeantFiles) {

## reinitialize variables

  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 0;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime = 00-00-00;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = 0;
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0;

    
## end of reinitialization

   $mdataSet  = ($$eachGeantFile)->dset;
   $mfName = ($$eachGeantFile)->filename;
   $mpath  = ($$eachGeantFile)->fpath;
   $mcTime  = ($$eachGeantFile)->timeS;
   $mprotc = ($$eachGeantFile)->faccess;
   $mowner = ($$eachGeantFile)->fowner; 
   $msize = ($$eachGeantFile)->dsize;
   $basename = basename("$mfName",".fzd");
   $basename =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;
   $mfileSeq = $2 +0;
   $mNevts = $3;
   $mrun =  $1;
   $mrunId = substr($1,3) + 0; 
   $mNevtLo = 1;
   $mNevtHi = $mNevts;
   $mformat = "fzd";
   $mcomp = "fzd"; 
   $msite = "hpss_rcf";
   $mhpss = "Y";
   $mtype = "MC"; 


## fill operation table in the database
   print "filling FileCatalog table with GEANT files \n";
   &fillDbTable();
 }

my @flsplit;
my $daqf;
   foreach $eachDaqFile (@hpssDaqFiles) {

## reinitialize variables


  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 999;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime = 00-00-00;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = 0;
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0;
  
    
## end of reinitialization

   $mdataSet  = ($$eachDaqFile)->dset;
   $mfName = ($$eachDaqFile)->filename;
   $mpath  = ($$eachDaqFile)->fpath;
   $mcTime  = ($$eachDaqFile)->timeS;
   $mprotc = ($$eachDaqFile)->faccess;
   $mowner = ($$eachDaqFile)->fowner; 
   $msize = ($$eachDaqFile)->dsize;
   $basename = basename("$mfName",".daq");
   $daqf =$basename;
  if($basename =~ /^st_/ ) {
   @flsplit = split ("_",$basename);  
   $mfileSeq = $flsplit[4];
   $mrun =  $flsplit[2];
   $mrunId = $mrun; 
 }
   else {
   $daqf =~ s/\./_/g;
    @flsplit = split ("_",$daqf); 
    $mrun =  $flsplit[1];
   if(defined $flsplit[2]) {
    $mfileSeq = $flsplit[2];
 }
   else {
   $mfileSeq = 0;
 } 
 }
    
   $mNevts = 10000;
   $mNevtLo = 1;
   $mNevtHi = $mNevts;
   $mformat = "daq";
   $mcomp = "daq"; 
   $msite = "hpss_rcf";
   $mhpss = "Y";
   $mtype = "online"; 

#   print "Daq name = ",$mfName, " Nrun = ", $mrun, "  FileSeq = ", $mfileSeq, "\n";

## fill operation table in the database
   print "filling FileCatalog table with DAQ files \n";
   &fillDbTable();
 }


## hpss reco file check
     foreach $eachRecoFile (@hpssRecoFiles) {

## reinitialize variables

  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 0;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime =00-00-00;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = "-rw-r-----";
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0; 

## end of reinitialization

my $mName;
my $chJobId;

   $mdataSet  = ($$eachRecoFile)->dset;
   $mfName = ($$eachRecoFile)->filename;
   $mpath  = ($$eachRecoFile)->fpath;
   $mcTime  = ($$eachRecoFile)->timeS;
   $mprotc = ($$eachRecoFile)->faccess;
   $mowner = ($$eachRecoFile)->fowner;
   $msize = ($$eachRecoFile)->dsize;
   $chJobId = ($$eachRecoFile)->fchain;
   $mJobId = "Job" ."_" . $chJobId;
   $mName = $mfName;
   $mName =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;  
   $mfileSeq = $2 ; 
   $mNevts = $3;
   $mrun = $1;
   $mrunId = substr($1,3) + 0;    
#   print "file name = ", $mName, "fileSeq = ", $mfileSeq, "%nrun = ", $mrunId, "\n"; 
   $mNevtLo = 1;
   $mNevtHi = $mNevts;
   if($mfName =~ /xdf/ ) {
     $mformat = "xdf";
     $mcomp = "dst";
   }
  elsif($mfName =~  /root/) {
     $mformat = "root";
     my $compont = basename("$mfName",".root");
     if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
     $mcomp = $1;
   } 
    elsif ( $mfName =~  /_dst.root/) {
     $mcomp = "dst";
    } else {
      $mcomp = "root-mdc2";
   } 
#  print "file name = ", $mfName, "  component =", $mcomp, "\n";
   }

   $msite = "hpss_rcf";
   $mhpss = "Y";
   $mtype = "MC_reco";  

## fill operation table in the database
    print "filling FileCatalog table with Reco files\n";
   &fillDbTable();

  }
my $extn; 
my $mfileS;
   foreach $eachDstFile (@hpssDstFiles) {

## reinitialize variables


  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 999;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime = 00-00-00;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = 0;
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0;

    
## end of reinitialization

   $mdataSet  = ($$eachDstFile)->dset;
   $mfName = ($$eachDstFile)->filename;
   $mpath  = ($$eachDstFile)->fpath;
   $mcTime  = ($$eachDstFile)->timeS;
   $mprotc = ($$eachDstFile)->faccess;
   $mowner = ($$eachDstFile)->fowner; 
   $msize = ($$eachDstFile)->dsize;
   $chJobId = ($$eachDstFile)->fchain;
   $mJobId = "Job" ."_" . $chJobId;
  if($mfName =~ /xdf/ ) {
     $mformat = "xdf";
     $mcomp = "dst";
  $basename = basename("$mfName",".xdf");
   }
  elsif($mfName =~  /root/) {
     $mformat = "root";
     $basename = basename("$mfName",".root");   
     my $compont = $basename;
     if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
     $mcomp = $1;
   }
   }
  if($basename =~ /^st_/ ) {
   @flsplit = split ("_",$basename);  
   $mfileS = $flsplit[4];
   $mrun =  $flsplit[2];
   $mrunId = $mrun;
   $extn = "." . $mcomp;
   $mfileSeq = basename("$mfileS","$extn"); 
 }
   else {
   $mfileS = $basename;
   $extn = "." . $mcomp;
   $daqf = basename("$mfileS","$extn");   
   $daqf =~ s/\./_/g;
    @flsplit = split ("_",$daqf); 
    $mrun =  $flsplit[1];
   if(defined $flsplit[2]) {
    $mfileSeq = $flsplit[2];
 }
   else {
   $mfileSeq = 0;
 } 
 }

   $mNevts = 10000;
   $mNevtLo = 1;
   $mNevtHi = $mNevts;
   $msite = "hpss_rcf";
   $mhpss = "Y";
   $mtype = "reco_daq"; 

   print "Dsq name = ",$mfName, "JOB_ID = ", $mJobId, " Nrun = ", $mrun, "  FileSeq = ", $mfileSeq, "\n";
   print "file name = ", $mfName, "  component =", $mcomp, "\n";
## fill operation table in the database
   print "filling FileCatalog table with DST files for DAQ data\n";
   &fillDbTable();
 }


 &StDbProdDisconnect();

 exit;


###########
sub fillDbTable {

    $sql="insert into $FilesCatalogT set ";
    $sql.="jobID='$mJobId',";
    $sql.="runID='$mrunId',";
    $sql.="fileSeq='$mfileSeq',";
    $sql.="eventType='$mevtType',";
    $sql.="fName='$mfName',";
    $sql.="path='$mpath',";
    $sql.="dataset='$mdataSet',";
    $sql.="size='$msize',";
    $sql.="createTime='$mcTime',";
    $sql.="Nevents='$mNevts',";
    $sql.="NevLo='$mNevtLo',";
    $sql.="NevHi='$mNevtHi',";
    $sql.="owner='$mowner',";
    $sql.="protection='$mprotc',";
    $sql.="type='$mtype',";
    $sql.="component='$mcomp',";
    $sql.="format='$mformat',";
    $sql.="site='$msite',"; 
    $sql.="hpss='Y',";
    $sql.="status= 0,";
    $sql.="comment=''";
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

  }

######################

sub walkHpss {

   my ( $ftp, $dirs, $files ) = @_;
 
 my @fields;
 my $access;
 my $downer;
 my $size;
 my $month;
 my $day;
 my $year;
 my $name; 
 my @dirF;
 my $fullDir;
 my $set; 
 
  for ($ii=0; $ii<$nHpssDirs; $ii++) {
    my @dird = $ftp->dir($dirs->[$ii]);

     for ($jj=0; $jj<@dird; $jj++) {
         @fields = split(/\s+/, $dird[$jj]);
         $access = $fields[0]; 
         $downer = $fields[2];
         $size   = $fields[4];
         $month  = $fields[5];
         $day    = $fields[6];
         $year   = $fields[7];
         $name   = $fields[8];
         $fullDir = $dirs->[$ii];
                   

         @dirF = split(/\//, $dirs->[$ii]); 

         $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
			                         $dirF[7],$dirF[8],$dirF[9]);

      $chainVal = "n\/a";
      if ( $dirF[10] ne "gstardata" && $dirF[10] ne "tfs_5" ) {
	$tpcMode = substr($dirF[10],0,3);
        $isYear = $dirF[8];   
#        print "prodPeriod =  ", $prodHash{$dirF[10]}, "\n";
	$chainVal = $prodHash{$dirF[10]} . "_" .$tpcMode."_".$isYear;

     }

      my $monthD = $monthHash{$month};
      my $sec = 0;
      my $min = 0;
      my $hr = 0;
      
      if ( $year =~ m/:/ ) {
	( $hr, $min ) = split(/:/,$year);
	$year = (localtime())[5];
      } else {
	$year = $year - 1900;
      }
      
      if( $year > 98 ) {
	$year = 1900 + $year;
      } else {
	$year = 2000 + $year;
      }
   
   
      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
			$year,$monthD,$day,$hr,$min);
   
#      $timeS = sprintf ("%4.4d%2.2d%2.2d",
#			$year,$monthD,$day);
      
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($fullDir);
      ($$fObjAdr)->dset($set);
      ($$fObjAdr)->fchain($chainVal);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);

      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
      print "File ".$name."\n" if $debugOn;
      }
    } 
   }

###==================================================================
sub walkDHpss {

   my ( $ftp, $dirs, $files ) = @_;
 
 my @fields;
 my $access;
 my $downer;
 my $size;
 my $month;
 my $day;
 my $year;
 my $name; 
 my @dirF;
 my $fullDir;
 my $set; 
 
  for ($ii=0; $ii<$nHpssDaqDirs; $ii++) {
    my @dird = $ftp->dir($dirs->[$ii]);

     for ($jj=0; $jj<@dird; $jj++) {
         @fields = split(/\s+/, $dird[$jj]);
         $access = $fields[0]; 
         $downer = $fields[2];
         $size   = $fields[4];
         $month  = $fields[5];
         $day    = $fields[6];
         $year   = $fields[7];
         $name   = $fields[8];
         $fullDir = $dirs->[$ii];
          
         next if ( $name =~ /^delete_me/);
         next if ( $name =~ /^file/);
         next if ( $name =~ /^ Star.Test/);
      
         @dirF = split(/\//, $dirs->[$ii]); 

      if($dirF[4] eq "daq")  {
        $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6]);
      }
        elsif ($dirF[4] eq "dst") {
        $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
                                                 $dirF[7]);
      }


      $chainVal = "n\/a";
      if ( $dirF[4] eq "dst") {
        $chainVal = $dirF[5] ."_" . "daq" . "_" . "year_1b";
        }

      my $monthD = $monthHash{$month};
      my $sec = 0;
      my $min = 0;
      my $hr = 0;
      
      if ( $year =~ m/:/ ) {
	( $hr, $min ) = split(/:/,$year);
	$year = (localtime())[5];
      } else {
	$year = $year - 1900;
      }
      
      if( $year > 98 ) {
	$year = 1900 + $year;
      } else {
	$year = 2000 + $year;
      }
   
   
      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
			$year,$monthD,$day,$hr,$min);
   
#      $timeS = sprintf ("%4.4d%2.2d%2.2d",
#			$year,$monthD,$day);
      
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($fullDir);
      ($$fObjAdr)->dset($set);
      ($$fObjAdr)->fchain($chainVal);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);

      $files->[$nHpssDaqFiles] = $fObjAdr;
      $nHpssDaqFiles++;
      print "File ".$name."\n" if $debugOn;
      }
    } 
}
