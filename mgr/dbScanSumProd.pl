#! /opt/star/bin/perl -w
#
# 
#
# 
#
# L.Didenko
#
# dbScanSumProd.pl
#
# Scanning FilesCatalog table to get production summary and put it to Web page
# 
################################################################################################

require "/afs/rhic/star/packages/DEV/mgr/dbCpProdSetup.pl";

#require "dbCpProdSetup.pl";

use File::Find;
use Class::Struct;

my $debugOn=0;

my @SetS = (
             "auau200/venus412/default/b0_3/year_1b/hadronic_on",
             "auau200/venus412/default/b3_6/year_1b/hadronic_on",
             "auau200/venus412/default/b6_9/year_1b/hadronic_on",
 );

my $SetD = "daq/1999/12/";

my $prod_html_back = "/star/u2e/starreco/prod5/summary/Prod5.summary\.html_back";
my $prod_html = "/star/u2e/starreco/prod5/summary/Prod5.summary\.html";

struct FilAttr => {
    dataS      => '$',
    flName     => '$',
    Nevts      => '$',
    flSize     => '$', 
    
};

my $prodSer = "prod5";

&beginHtml();

## connect to the DB
&StDbProdConnect();

## Find sets in DataSet table

my %InEvts = ();
my %InSize = ();
my %dstDsize= ();
my %dstHpsize = ();
my %dstDEvts = ();
my %dstHpEvts = ();

### select Geant files from FileCatalog
my $nmfile;
my @hpssInFiles;

$nhpssInFiles = 0;

my $seti;
 for($ik=0; $ik <scalar(@SetS); $ik++) {

$sql="SELECT dataset, fName, Nevents, size  FROM $cpFileCatalogT WHERE dataset = '$SetS[$ik]' AND fName LIKE '%fzd' AND hpss = 'Y'";
$cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

while(@fields = $cursor->fetchrow) {
  my $cols=$cursor->{NUM_OF_FIELDS};
  $fObjAdr = \(FilAttr->new());

  for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
    my $fname=$cursor->{NAME}->[$i];
    print "$fname = $fvalue\n" if $debugOn;

    ($$fObjAdr)->flName($fvalue)    if( $fname eq 'fName');
    ($$fObjAdr)->Nevts($fvalue)     if( $fname eq 'Nevents');
    ($$fObjAdr)->flSize($fvalue)    if( $fname eq 'size');
    ($$fObjAdr)->dataS($fvalue)     if( $fname eq 'dataset');
  }
  
  $hpssInFiles[$nhpssInFiles] = $fObjAdr;  
  $nhpssInFiles++;
   
 }
}
my $nmfile;
my $dtSet;
foreach my $pfile (@hpssInFiles) {

   $nmfile = ($$pfile)->flName;
    next  if( $nmfile =~ /^psc/);
   $dtSet = ($$pfile)->dataS;
   $InEvts{$dtSet}  += ($$pfile)->Nevts;
   $InSize{$dtSet}  += ($$pfile)->flSize;  

 }

### select DST files on HPSS from cpFileCatalog
my $hdfile;
my $dhSet;

my @hpssDstFiles;
$nhpssDstFiles = 0;

for ($ll=0; $ll<scalar(@SetS); $ll++) {

$sql="SELECT dataset, fName, Nevents, size  FROM $cpFileCatalogT WHERE dataset = '$SetS[$ll]' AND fName LIKE '%dst.root' AND JobID LIKE '%prod5%' AND hpss ='Y'";
$cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

while(@fields = $cursor->fetchrow) {
  my $cols=$cursor->{NUM_OF_FIELDS};
  $fObjAdr = \(FilAttr->new());

  for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
    my $fname=$cursor->{NAME}->[$i];
    print "$fname = $fvalue\n" if $debugOn;
    ($$fObjAdr)->flName($fvalue)    if( $fname eq 'fName');
    ($$fObjAdr)->Nevts($fvalue)   if( $fname eq 'Nevents');
    ($$fObjAdr)->flSize($fvalue)   if( $fname eq 'size');
    ($$fObjAdr)->dataS($fvalue)   if( $fname eq 'dataset');
  }
  
  $hpssDstFiles[$nhpssDstFiles] = $fObjAdr;  
  $nhpssDstFiles++;
   
  }
}
foreach my $dsfile (@hpssDstFiles) {

   $dhfile = ($$dsfile)->flName;
   $dhSet = ($$dsfile)->dataS;
   $dstHpEvts{$dhSet}  += ($$dsfile)->Nevts;
   $dstHpsize{$dhSet}  += ($$dsfile)->flSize; 

 }

### select DST files on DISK from cpFileCatalog

my $ddfile;
my $ddSet;
my @diskDstFiles;

$ndiskDstFiles = 0;

for ($kk=0; $kk<scalar(@SetS); $kk++) {

$sql="SELECT dataset, fName, Nevents, size  FROM $cpFileCatalogT WHERE dataset = '$SetS[$kk]' AND fName LIKE '%dst.root' AND JobID LIKE '%prod5%' AND site = 'disk_rcf'";
$cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

while(@fields = $cursor->fetchrow) {
  my $cols=$cursor->{NUM_OF_FIELDS};
  $fObjAdr = \(FilAttr->new());

  for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
    my $fname=$cursor->{NAME}->[$i];
    print "$fname = $fvalue\n" if $debugOn;

    ($$fObjAdr)->flName($fvalue)    if( $fname eq 'fName');
    ($$fObjAdr)->Nevts($fvalue)   if( $fname eq 'Nevents');
    ($$fObjAdr)->flSize($fvalue)   if( $fname eq 'size');
    ($$fObjAdr)->dataS($fvalue)   if( $fname eq 'dataset');
  }
  
  $diskDstFiles[$ndiskDstFiles] = $fObjAdr;  
  $ndiskDstFiles++;
   
  }
}
foreach my $ddfile (@diskDstFiles) {

   $dhfile = ($$ddfile)->flName;
   $ddSet = ($$ddfile)->dataS;
   $dstDEvts{$ddSet}  += ($$ddfile)->Nevts;
   $dstDsize{$ddSet}  += ($$ddfile)->flSize;   

 }

#initialize for total amount

my $TInSize = 0;
my $TdstDsize = 0;
my $TdstHpsize = 0;
my $TInEvt = 0;
my $TdstDEvt  = 0; 
my $TdstHEvt  = 0;

  
# get giga number for dst and raw files and accumulate total amount

  foreach my $setE (@SetS) {
      $InSize{$setE}   = int($InSize{$setE}/1000000000);
      $dstDsize{$setE}  = int($dstDsize{$setE}/1000000000);   
      $dstHpsize{$setE} = int($dstHpsize{$setE}/1000000000);
      $TInSize     +=  $InSize{$setE};
      $TInEvt      +=  $InEvts{$setE};
      $TdstDsize   +=  $dstDsize{$setE};
      $TdstHpsize  +=  $dstHpsize{$setE};
      $TdstHEvt    +=  $dstHpEvts{$setE};   
      $TdstDEvt    +=  $dstDEvts{$setE}; 


  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<TD><a href=\"http://duvall.star.bnl.gov/devcgi/dbFileCatalogRetrv.pl?set=$setE\">$setE</TD>\n"; 
  print HTML "<td>$InSize{$setE}</td><td>$InEvts{$setE}</td><td>$dstHpsize{$setE}</td><td>$dstHpEvts{$setE}</td><td>$dstDsize{$setE}</td><td>$dstDEvts{$setE}</td></tr>\n"; 

}

# print total amount
  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<td>Total for prod5 </td>\n"; 
  print HTML "<td>$TInSize </td><td>$TInEvt </td><td>$TdstHpsize </td><td>$TdstHEvt</td><td>$TdstDsize </td><td>$TdstDEvt </td></tr>\n"; 

##


# finished with database
&StDbProdDisconnect();

&endHtml();

`cp $prod_html_back $prod_html` ;

######################
sub beginHtml {

  open (HTML,">$prod_html_back") or die "can't write to $prod_html_back ";
  print HTML "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n";
  print HTML "<html>\n";
  print HTML "  <head>\n";
  print HTML "          <title>Prod5 Series Production summary </title>\n";
  print HTML "  </head>\n";
  print HTML "  <body BGCOLOR=\"#ccffff\"> \n";
  print HTML "      <h1>Prod5 Series Production summary</h1>\n";
  print HTML "<TABLE BORDER=5 CELLSPACING=1 CELLPADDING=2 >\n";
  print HTML "<TR>\n";
  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<TD WIDTH=\"20%\" HEIGHT=110><B>DataSet</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>GEANT input<br>size (GB)</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>GEANT input <br>No. of events</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst on HPSS<br>size (GB)</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst on HPSS<br>No.of events </B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst on DISK<br>size (GB)</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst on DISK<br>No.of events </B></TD>\n";
  print HTML "</TR>\n";
}

#####################
sub endHtml {
  my $Date = `date`;
  
  print HTML "</TABLE>\n";
  print HTML "      <h5>\n";
  print HTML "      <address><a href=\"mailto:didenko\@bnl.gov\">Lidia Didenko</a></address>\n";
  print HTML "<!-- Created: Mon Nov 29  05:29:25 MET 1999 -->\n";
  print HTML "<!-- hhmts start -->\n";
  print HTML "Last modified: $Date\n";
  print HTML "<!-- hhmts end -->\n";
  print HTML "  </body>\n";
  print HTML "</html>\n";
  close (HTML);
}












