#!/usr/bin/perl

use strict;
use warnings;

use lib '/star/u/dkap7827/Tools2/Tools/PerlScripts';
use StarFileList;
use Cwd qw(abs_path);
use Getopt::Long qw(GetOptions);
use Getopt::Long qw(HelpMessage);
#use Getopt::Long qw(VesrionMessage);
use Pod::Usage;

=pod

=head1 NAME

MakeRunList - Handy script for generating the run list to use for Fcs Run 22 Analysis. For this reason certain search conditions are fixed

=head1 SYNOPSIS

MakeRunList.pl

  --keys, -k     Keys to return from search (default path,filename,events,size)
  --file, -f     File to write output to (default is 'test.list')
  --limit, -l    Number of files to return (default is 10)
  --print, -p    Just print to terminal the commands or file list, will not create a file  (1=command, 2=command and list)
  --verbose, -v  Set the printout level (default is 0)
  --help, -h     Print this help

=head1 VERSION

0.3

=head1 PURPOSE

Generate a run list for Run 22 A_N analysis with P24ia production tag.

=head2 DESCRIPTION

This program will generate a run list of MuDst files to use for Run 22 analysis. It will use "StarFileList" to call the neccessary methods to do this. Certain search conditions will be fixed since this makes it easier to select the relevant runs.

=cut

=begin comment
@[December 7, 2023](David Kapukchyan)
> First instance (v0.1)

@[December 15, 2023](David Kapukchyan)
> Added some more command line options and cleaned up a little bit (v0.2)

@[July 9, 2024](David Kapukchyan)
> Added more relevant search criteria for Run 22 P24ia production (v0.3)
@[August 29, 2024](David Kapukchyan)
> Added *size* to default key option
=cut


my $KEYS = "path,filename,events,size";
#my $CONDITIONS = "";
my $FILE = "test.list";
my $LIMIT = 10;
my $PRINTLEVEL = 0;
my $VERBOSE = 0;

GetOptions(
    'keys|k=s'    => \$KEYS,
    'file|f=s'    => \$FILE,
    'limit|l=i'   => \$LIMIT,
    'print|p=i'   => \$PRINTLEVEL,
    'verbose|v=i' => \$VERBOSE,
    'help|h'      => sub { HelpMessage() }
    ) or HelpMessage(1);


my $FileList = new StarFileList();
$FileList->Keys($KEYS);
$FileList->Storage("=local"); # This is files on distributed disk
$FileList->Trgsetupname("=production_pp500_2022");
$FileList->Filetype("=daq_reco_mudst");
#$FileList->Production("=P23if");
$FileList->Production("=P24ia");   # @[September 1, 2024] > Run 22 st_fwd production, no TPC, no fwd tracking
#$FileList->Production("=P25ib");    # @[February 20, 2026] > Run 22 st_physics pre-production
$FileList->Sname2("=st_fwd");
#$FileList->Sname2("=st_physics");
#$FileList->Runnumber(">23005043");         #Before this run ECAL not calibrated
#$FileList->Runnumber("=23007011");
$FileList->Runnumber(">=23050001");        #Before this run (exclusive) no StMuEpdHits in MuDsts for P24ia production
#$FileList->Runnumber(">=23108002");        #For cross-checking the spin database
$FileList->AddExtraOpts("-limit $LIMIT");

#Get Time
#my $epochtime = time();            #UNIX time (seconds from Jan 1, 1970)
#my $localtime = localtime();       #Human readaable time
if( $PRINTLEVEL>2 ){ die "ERROR:Invalid print level:'$PRINTLEVEL'\n"; }
if($PRINTLEVEL==1 ){  $FileList->Command(1); exit; }

if( $VERBOSE>=1 ){
    print "Keys:",$FileList->Keys(),"\n";
    print "Conditions:",$FileList->Conditions(),"\n";
}

my $FileArrRef = $FileList->RetrieveModFiles();

$FileList->SortByRunNumber();

if($PRINTLEVEL==2 ){ $FileList->PrintFiles(); exit; }

my $filesize = @{$FileArrRef};
if( $VERBOSE>=1 ){ $FileList->PrintFiles(); }

$FileList->WriteFiles($FILE);

