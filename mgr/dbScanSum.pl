#! /opt/star/bin/perl -w
#
# 
#
# 
#
# L.Didenko
#
# dbScanSum.pl
#
# Scanning DstProduction table to get production summary and put it to Web page
# 
################################################################################################

require "/afs/rhic/star/packages/SL99i/mgr/dbDstProdSetup.pl";
#require "dbDstProdSetup.pl";

use Class::Struct;


my $prod_html_back = "/star/u2e/starreco/prod4/summary/Prod4_Dstsum\.html_back";
my $prod_html = "/star/u2e/starreco/prod4/summary/Prod4_Dstsum\.html";

struct FilAttr => {
    numRun     => '$',
    fName      => '$',
    evType     => '$',
    daqEvt     => '$', 
    dqSize     => '$',
    dskSize    => '$',
    hpSize     => '$',
    dstEvt     => '$',
    
};

struct  JOptAttr =>  {
          prodSer  => '$',
          evtType  => '$', 

};

my $debugOn=1;

&beginHtml();

## connect to the DB
&StDbDstProdConnect();

## Find sets in DataSet table

my @setEvt;
my @listEvt;
my %rawEvt = ();
my %rawSize = ();
my %dstDsize= () ;
my %dstHpsize = ();
my %dstEv = ();
my $nEvts = 0;
my $nlistEv = 0;
my @prodSet;
my @listProd;
my $nlistPr = 0;
my $nseries = 0;

my $ii = 0;

my @prodFiles;
$npfiles = 0;


$sql="SELECT prodSeries, eventType FROM $ProdOptionsT";

  $cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute;
 
 while(@fields = $cursor->fetchrow) {
   my $cols=$cursor->{NUM_OF_FIELDS};
       $jObAdr= \(JOptAttr->new()); 

  for($i=0;$i<$cols;$i++) {
   my $fvalue=$fields[$i];
     my $fname=$cursor->{NAME}->[$i];
#    print "$fname = $fvalue\n" if $debugOn;

        ($$jObAdr)->prodSer($fvalue)    if( $fname eq 'prodSeries');
        ($$jObAdr)->evtType($fvalue)    if( $fname eq 'eventType');
 }
        $listEvt[$nlistEv]   = ($$jObAdr)->evtType ;
        $listProd[$nlistPr]  = ($$jObAdr)->prodSer ;    
        $nlistEv++;
        $nlistPr++;  
}

my $ll = 0;
$nEvts = 1;

      $setEvt[0] = $listEvt[0];
if($nlistEv ge 2 ) {
    for( $ll=1; $ll<$nlistEv; $ll++) {
        if(!($listEvt[$ll] eq $listEvt[$ll-1]))  {
           $setEvt[$nEvts] = $listEvt[$ll];      
           $nEvts++;
     }
  }
}  
$ll = 0;
      $prodSet[0] = $listProd[0];
if($nlistPr ge 2) {
  for ( $ll=1; $ll<$nlistPr; $ll++) {
	  if(!($listProd[$ll] eq $listProd[$ll-1])) {
                $prodSet[$nseries] = $listProd[$ll];
# print $prodSet[$nseries], "\n";
                 $nseries++;
	      }
       }
  }
   

my $setE ;
my $jj = 0;
my $pfile;
my $nnev = 0;
my $setEv ;

# for ($jj=0; $jj<$nseries; $jj++) {

$npfiles = 0;

foreach $setE (@setEvt) {
#       $setE = $setEv; 
      $rawEvt{$setE} = 0;
      $rawSize{$setE} = 0;
      $dstDsize{$setE} = 0;
      $dstHpsize{$setE}= 0;
      $dstEv{$setE}= 0; 
#   print $setE, "\n"; 
}      
              

$sql="SELECT nrun, fileName, eventType, daqEvents, daq_size, disk_size, HPSS_size, dstEvents  FROM $DstProductionT WHERE prodSeries = '$prodSet[$jj]'";
$cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

while(@fields = $cursor->fetchrow) {
  my $cols=$cursor->{NUM_OF_FIELDS};
  $fObjAdr = \(FilAttr->new());

  for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
    my $fname=$cursor->{NAME}->[$i];
#    print "$fname = $fvalue\n" if $debugOn;
    ($$fObjAdr)->numRun($fvalue)   if( $fname eq 'nrun');
    ($$fObjAdr)->fName($fvalue)    if( $fname eq 'fileName');
    ($$fObjAdr)->evType($fvalue)   if( $fname eq 'eventType');
    ($$fObjAdr)->daqEvt($fvalue)   if( $fname eq 'daqEvents');
    ($$fObjAdr)->dqSize($fvalue)   if( $fname eq 'daq_size');
    ($$fObjAdr)->dskSize($fvalue)  if( $fname eq 'disk_size');
    ($$fObjAdr)->hpSize($fvalue)   if( $fname eq 'HPSS_size');
    ($$fObjAdr)->dstEvt($fvalue)   if( $fname eq 'dstEvents');
  }
  
  $prodFiles[$npfiles] = $fObjAdr;  
  $npfiles++;
   
}

##

$nnev = 0;

foreach $pfile (@prodFiles) {

   $nmfile = ($$pfile)->fName;
   $typeEv = ($$pfile)->evType;
   $rawEvt{$typeEv}   += ($$pfile)->daqEvt;
   $rawSize{$typeEv}  += ($$pfile)->dqSize;   
   $dstDsize{$typeEv} += ($$pfile)->dskSize;
   $dstHpsize{$typeEv}+= ($$pfile)->hpSize;
   $dstEv{$typeEv}    += ($$pfile)->dstEvt; 
     $nnev++;

 }

#initialize for total amount

my $TrawSize = 0;
my $TdstDsize = 0;
my $TdstHpsize = 0;
my $TrawEvt = 0;
my $TdstEv  = 0; 

  
# get giga number for dst and raw files and accumulate total amount

  foreach $setE (@setEvt) {
      $rawSize{$setE}   = $rawSize{$setE}/1000000000;
      $dstDsize{$setE}  = $dstDsize{$setE}/1000000000;   
      $dstHpsize{$setE} = $dstHpsize{$setE}/1000000000;
      $TrawSize    +=  $rawSize{$setE};
      $TrawEvt     +=  $rawEvt{$setE};
      $TdstDsize   +=  $dstDsize{$setE};
      $TdstHpsize  +=  $dstHpsize{$setE};
      $TdstEv      +=  $dstEv{$setE}; 


  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<td>$prodSet[$jj]</td>\n"; 
  print HTML "<td>$setE</td><td>$rawSize{$setE}</td><td>$rawEvt{$setE}</td><td>$dstHpsize{$setE} </td><td>$dstDsize{$setE}</td><td>$dstEv{$setE}</td></tr>\n"; 

}

# print total amount
  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<td>Total for $prodSet[$jj] </td>\n"; 
  print HTML "<td>all Events </td><td>$TrawSize </td><td>$TrawEvt </td><td>$TdstHpsize </td><td>$TdstDsize </td><td>$TdstEv </td></tr>\n"; 

##

# }   # end of $jj loop

# finished with database
&StDbDstProdDisconnect();

&endHtml();

`cp $prod_html_back $prod_html` 

######################
sub beginHtml {

  open (HTML,">$prod_html_back") or die "can't write to $new_html";
  print HTML "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n";
  print HTML "<html>\n";
  print HTML "  <head>\n";
  print HTML "          <title>DST Production summary</title>\n";
  print HTML "  </head>\n";
  print HTML "  <body BGCOLOR=\"#ccffff\"> \n";
  print HTML "      <h1>Production summary</h1>\n";
  print HTML "<TABLE BORDER=5 CELLSPACING=1 CELLPADDING=2 >\n";
  print HTML "<TR>\n";
  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>Production Series</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>Event type</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>daq file<br>size (GB)</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>daq <br>No.of events</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst on HPSS<br>size (GB)</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst on disk<br>size (GB)</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>dst <br>No.of events </B></TD>\n";
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












