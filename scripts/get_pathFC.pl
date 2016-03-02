#!/opt/star/bin/perl -w
#
# script to get path name from daq file
# L. Didenko
###########################################################################################

use lib "/afs/rhic/star/packages/scripts";
use FileCatalog;

my $fileC = new FileCatalog();


my $daqfile = $ARGV[0];
my @arpath = ();
my $pathname;

my $SITE = "BNL";
my $status       = (0==1);

$fileC->connect_as($SITE."::User","FC_user") || die "Connection failed for FC_user\n";

$fileC->set_context("filename=$daqfile","storage=HPSS"); # set conditions

@arpath   = $fileC->run_query("trgsetupname","path");

$pathname = $arpath[0];

$fileC->clear_context();

if( defined  $pathname) {

print $pathname, "\n";

}else{

print "File $daqfile was not found ", "\n";

}

exit;
