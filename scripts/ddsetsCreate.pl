#!/usr/bin/env perl
#
# $Log:
# L.Didenko
#
# ddsetsCreate.pl - scripts to create html files for stream data summary for each production sets on DD
#
########################################################################################################

use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use FileCatalog;

use DBI;

my $SITE         = "BNL";
my $status       = (0==1);


my $fileC = new FileCatalog();

my @prodset = ();
my @setname = ();
my @prodtag = ();
my @sumfile = ();
my $nlist = 0;
my @prt = ();
my $flsize = 0;


 $fileC->connect_as($SITE."::User","FC_user") || die "Connection failed for FC_user\n";

 $fileC->set_context("filetype=daq_reco_MuDst","storage=local","limit=0");

 @prodset = $fileC->run_query("trgsetupname","ordd(production)");

 $fileC->clear_context( );

 `cd /star/u/starlib/localdata`;
 `rm /star/u/starlib/localdata/*loc.html`;
 
    foreach my $line (@prodset){

#    print $line , "\n";
    @prt = ();
    @prt = split("::",$line);

    $setname[$nlist] = $prt[0];
    $prodtag[$nlist] = $prt[1];

    if($setname[$nlist] eq "ProductionMinBias" ) {
	$setname[$nlist] =~ s/P/p/g ;
    }

   $sumfile[$nlist] = $setname[$nlist].".".$prodtag[$nlist]."_loc.html";


`wget --user=protected --password=#1,Engage! 'http://www.star.bnl.gov/devcgi/RetriveStreamDd.pl?trigs=$setname[$nlist];prod=$prodtag[$nlist]' -O $sumfile[$nlist]`;

   $flsize = (stat($sumfile[$nlist]))[7];
   if($flsize > 1000) {    

 `cp /star/u/starlib/localdata/$sumfile[$nlist]  /afs/rhic.bnl.gov/star/doc/www/comp/prod/localdata` ;

    print "File  ", $sumfile[$nlist], "  copied to /afs/rhic.bnl.gov/star/doc/www/comp/prod/localdata","\n";

  }else{
    print "File  ", $sumfile[$nlist], "  has size = $flsize","\n";
  }
 
    $nlist++;
  }

   $fileC->destroy();

exit;
