#! /opt/star/bin/perl -w
#
# Report production job failures from mySQL dB to HTML file
# Author: Lee Barnby - Kent State University (@BNL)
# Date: June 2000 
#
############################################################################


require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";
use Class::Struct;

## Output html (plus temporary backup) file names
my $prod_html_back = "/star/u2e/starreco/mdc3/summary/jobfailure.summary\.html_back";
my $prod_html = "/star/u2e/starreco/mdc3/summary/jobfailure.summary\.html";
my $file_root = "/star/u2e/starreco/mdc3/summary";
my $html_root = "~starreco/mdc3/summary";

## Set higher debug levels to get information printed to std out
my $debugLevel=0;

## Specific types of failures can be added or deleted here
my @failureTypes = (
		    "Run not completed",
		    "segmentation violation",
		    "Assertion failed",
		   );
### Temporary shorter list for testing purposes
#local @SetS = (
#             "auau200/mevsim/vanilla/central/year_1h/hadronic_on",
#             "auau200/mevsim/cascade/central/year_1h/hadronic_on",
#             "auau200/mevsim/vanilla/flow/year_1h/hadronic_on", 
#	   );

## List of datasets
local @SetS = (
             "auau200/mevsim/vanilla/central/year_1h/hadronic_on",
             "auau200/mevsim/cascade/central/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/flow/year_1h/hadronic_on", 
             "auau200/mevsim/vanilla/fluct/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/resonance/year_1h/hadronic_on", 
             "auau200/mevsim/vanilla/trigger/year_1h/hadronic_on",  
             "auau200/nexus/default/b0_3/year_1h/hadronic_on",
             "auau200/hijing/b8_15_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing/b8_15_jetq_on/jet05/year_1h/hadronic_on",
             "auau200/hijing/b0_3_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing/b0_3_jetq_on/jet05/year_1h/hadronic_on",
             "auau200/hbt/default/peripheral/year_1h/hadronic_on",
             "auau200/hbt/default/midperipheral/year_1h/hadronic_on",
             "auau200/hbt/default/middle/year_1h/hadronic_on",
             "auau200/hbt/default/central/year_1h/hadronic_on",
             "auau200/hbt/default/midcentral/year_1h/hadronic_on", 
             "auau200/vni/default/b0_3/year_1h/hadronic_on",
             "auau200/vni/default/b0_3/year_1h1/hadronic_on",
             "auau200/starlight/2gamma/halffield/year_1h/hadronic_on",
             "auau200/starlight/2gamma/none/year_1h/hadronic_on",
             "auau200/starlight/vmeson/halffield/year_1h/hadronic_on",
             "auau200/starlight/vmeson/none/year_1h/hadronic_on",             
             "auau200/dtunuc/two_photon/halffield/year_1h/hadronic_on",
             "auau200/dtunuc/two_photon/none/year_1h/hadronic_on",
             "auau200/hemicosm/default/none/year_1h/hadronic_on",
             "auau200/hijing/beamgas/hydrogen/year_1h/hadronic_on",
             "auau200/hijing/beamgas/nitrogen/year_1h/hadronic_on", 
             "pau200/hijing/b0_7/gam15/year_1h/hadronic_on",
             "pau200/hijing/b0_7/jet15/year_1h/hadronic_on",
             "pau200/hijing/b0_7/gam15/year_2a/hadronic_on",
             "pau200/hijing/b0_7/jet15/year_2a/hadronic_on", 
             "auau200/mevsim/vcascade/central/year_1h/hadronic_on",       
             "auau200/mevsim/vcascade/flow/year_1h/hadronic_on", 
             "auau200/mevsim/vcascade/fluct/year_1h/hadronic_on",
             "auau200/mevsim/vcascade/resonance/year_1h/hadronic_on", 
             "auau200/mevsim/vcascade/trigger/year_1h/hadronic_on",
             "auau200/hijing_quark/b0_3_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing_quark/b0_3_jetq_on/jet05/year_1h/hadronic_on",
             "auau200/hijing_antinuc/b0_3_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing_antinuc/b0_3_jetq_on/jet05/year_1h/hadronic_on",
             "pp200/pythia/default/minibias/year_2a/hadronic_on",           
 );

struct JobInfo => {
		   jobID => '$',
		   nEvents => '$',
		   status => '$',
		   fileName => '$',
		  };

## Initialization
&beginHtml();
&StDbProdConnect();


## Start of loop over datasets . Am using 'for' rather than 'foreach' so that
## there is an index number which can be used for suffixing file names etc.
local $iSet;
for($iSet=0; $iSet < scalar(@SetS); $iSet++) {
  
  my %thisSetJobs; #hash with jobID and number of occurences (=no. of files)
  $nthisSetJobs=0;
  local @badJobs; #array of (addresses of) JobInfo structures
  $nbadJobs=0;
  my %failures; #hash with name of failure and no. of occurences
  
  ## Get all job IDs for this dataset from FileCatalog table.
  $sql="SELECT JobID FROM $FileCatalogT WHERE dataset = '$SetS[$iSet]'";
  $cursor =$dbh->prepare($sql) 
    || die "Cannot prepare statement: $DBI::errstr\n";
  $cursor->execute;
  while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
    for($i=0;$i<$cols;$i++) {
      my $fvalue=$fields[$i];
      my $fname=$cursor->{NAME}->[$i];
      if( $fname eq 'JobID') {
	if (! defined $thisSetJobs{$fvalue}) {$thisSetJobs{$fvalue} = 0};
	$thisSetJobs{$fvalue}++;
	$nthisSetJobs++;
      } 
    }
  }
  ## Hash "thisSetJobs" now has jobIDs for all jobs in this dataset along with
  ## the number of times each occurs (equivalent to number of files?)
  if ($debugLevel > 1) {
    while ( ($j,$n) = each %thisSetJobs) {
      print "Debug message: job $j, $n files \n";}       
  }
  
  ## Use jobIDs to get information from JobStatus table. We are not interested
  ## in those with a status of 'n/a' or 'Done'
  while ( ($job,$n) = each %thisSetJobs) {
    $sql="SELECT JobID, jobStatus, sumFileName, NoEvents FROM $JobStatusT WHERE JobID = '$job' AND jobStatus != 'Done' AND jobStatus != 'n/a'";
    $cursor =$dbh->prepare($sql) 
      || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;
    while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};
      $objAddr = \(JobInfo->new());
      for($i=0;$i<$cols;$i++) {
	my $fvalue=$fields[$i];
	my $fname=$cursor->{NAME}->[$i];
	
	($$objAddr)->jobID($fvalue) if( $fname eq 'JobID');
	($$objAddr)->nEvents($fvalue) if( $fname eq 'NoEvents');
	($$objAddr)->fileName($fvalue) if( $fname eq 'sumFileName');
	if( $fname eq 'jobStatus'){
	  ($$objAddr)->status($fvalue);
	  $failures{$fvalue} += 1;
	}
      }
      $badJobs[$nbadJobs] = $objAddr;
      $nbadJobs++;
    }
  } ##end loop over jobs
  
  local $idlist_html_back="$file_root/Set_$iSet\.html_back";
  local $idlist_html="$file_root/Set_$iSet\.html";
  my $html_link="Set_$iSet\.html";

  ## Write information into row of html table
  my $nspecified=0;
  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  if ($nbadJobs > 0){
    print HTML "<TD><a href=\"$html_link\">$SetS[$iSet]</TD>\n";
  }
  else {
    print HTML "<TD>$SetS[$iSet]</TD>\n";
  }
  print HTML "<TD>$nbadJobs</TD>\n";
  foreach my $ftype (@failureTypes) {
    if(!defined $failures{$ftype}) {$failures{$ftype}= 0}
    print HTML "<TD>$failures{$ftype}</TD>\n";
    $nspecified+=$failures{$ftype};
  }
  my $nother = $nbadJobs - $nspecified;
  print HTML "<TD>$nother</TD>\n";
  print HTML "</TR>\n";
  
  ## Make the html file with details for this dataset
  if($nbadJobs > 0){
    &detailHtml();
  }

  ## Print out failed job information for debugging purposes
  if ($debugLevel){
    while (($failuretype,$num) = each(%failures)) {
      print "Failure type $failuretype happened $num times \n";
    }
    print "number of bad jobs is $nbadJobs \n";
    foreach my $badjob (@badJobs) {
      my $ID = ($$badjob)->jobID;
      my $status = ($$badjob)->status;
      print "$ID failed because of $status \n";
    }
  }
} ##End of loop over datasets


&StDbProdDisconnect();
&endHtml();
`cp $prod_html_back $prod_html` ;

## The End ##


######################
sub beginHtml {
  open (HTML,">$prod_html_back") or die "can't write to $prod_html_back ";
  print HTML "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n";
  print HTML "<html>\n";
  print HTML "  <head>\n";
  print HTML "          <title> Job failure summary </title>\n";
  print HTML "  </head>\n";
  print HTML "  <body BGCOLOR=\"#ccffff\"> \n";
  print HTML "      <h1>Job failure summary</h1>\n";
  print HTML "<TABLE BORDER=5 CELLSPACING=1 CELLPADDING=2 >\n";
  print HTML "<TR>\n";
  print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print HTML "<TD WIDTH=\"20%\" HEIGHT=110><B>DataSet</B></TD>\n";
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>Total number of failures</B></TD>\n";
  foreach my $ftype (@failureTypes){
    print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>$ftype</B></TD>\n";
  }
  print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>other</B></TD>\n";
  print HTML "</TR>\n";
}

#####################
sub endHtml {
  my $Date = `date`;
  
  print HTML "</TABLE>\n";
  print HTML "      <h5>\n";
  print HTML "      <address><a href=\"mailto:lbarnby\@bnl.gov\">Lee Barnby</a></address>\n";
  print HTML "<!-- hhmts start -->\n";
  print HTML "Last modified: $Date\n";
  print HTML "<!-- hhmts end -->\n";
  print HTML "  </body>\n";
  print HTML "</html>\n";
  close (HTML);
}

#####################
sub detailHtml {
  open (IDHTML,">$idlist_html_back") or die "can't write to $idlist_html_back";
  print IDHTML "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n";
  print IDHTML "<html>\n";
  print IDHTML "  <head>\n";
  print IDHTML "          <title> $SetS[$iSet] Details </title>\n";
  print IDHTML "  </head>\n";
  print IDHTML "  <body BGCOLOR=\"#ccffff\"> \n";
  print IDHTML "      <h3>Jobs failing from dataset <p> $SetS[$iSet]</h3>\n";
  print IDHTML "<TABLE BORDER=5 CELLSPACING=1 CELLPADDING=2 >\n";
  print IDHTML "<TR>\n";
  print IDHTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
  print IDHTML "<TD><B>Log File Name</B></TD>\n";
  print IDHTML "<TD><B>JobStatus</B></TD>";
  print IDHTML "<TD><B>Number of Events</B></TD>";
  print IDHTML "</TR>\n";
  foreach my $pinfo (@badJobs){
    print IDHTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
    my $name = ($$pinfo)->fileName; $name =~ s/sum/log/g;
    print IDHTML "<TD>$name</TD>\n";
    my $status = ($$pinfo)->status;
    print IDHTML "<TD>$status</TD>\n";
    my $nEvents = ($$pinfo)->nEvents;
    print IDHTML "<TD>$nEvents</TD>\n";
    print IDHTML "<\TR>\n";
  }
  print IDHTML "</TABLE>\n";
  print IDHTML "  </body>\n";
  print IDHTML "</html>\n";
  close (IDHTML);
  `cp $idlist_html_back $idlist_html`;
}

