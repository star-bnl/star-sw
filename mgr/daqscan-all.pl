#!/opt/star/bin/perl
#
# $Id: daqscan-all.pl,v 1.3 2000/01/26 15:58:24 wenaus Exp $
#
# $Log: daqscan-all.pl,v $
# Revision 1.3  2000/01/26 15:58:24  wenaus
# rhic.bnl.gov -> rcf.bnl.gov
#
# Revision 1.2  1999/09/21 12:25:36  wenaus
# Update bad file list
#
# Revision 1.1  1999/08/19 16:35:41  wenaus
# Runs scanner on disk-resident DAQ files to load DB info
#
#
######################################################################
#
# daqscan-all.pl
#
# Torre Wenaus 8/99
#
# Runs scanner on disk-resident DAQ files to load DB info
#
# Usage: From wenaus account!
#

use lib "/star/u2d/wenaus/datadb";
require "dbsetup.pl";

$debugOn = 0;

# Data locations, and the machine from which the location should be scanned
%dataDirs = (
             '/disk1/star/daq' => 'rmds03.rcf.bnl.gov',
             '/star/datapool/1/daq' => 'robinson.star.bnl.gov'
             );
# Files which hang in StDaqLib or otherwise screw up
%badFiles = (
             '990618.107.daq' => 1,
             '990624.301.daq' => 1,
             '990624.302.daq' => 1,
             '990624.303.daq' => 1,
             '990624.304.daq' => 1,
             '990624.306.daq' => 1,
             '990624.308.daq' => 1,
             '990624.309.daq' => 1,
             '990719.2503.daq' => 1,
             '990719.2521.daq' => 1,
             'st_physics_0003459_raw_0001.daq' => 1,
             'st_physics_0003459_raw_0002.daq' => 1,
             'st_physics_0003460_raw_0001.daq' => 1,
             'st_physics_0003452_raw_0001.daq' => 1,
             'st_physics_0003455_raw_0001.daq' => 1,
             'st_physics_0003455_raw_0002.daq' => 1
             );

&StDbConnect();
# Collect the run file names out of the DB
$sql="select runname,events from $RunFileT";
$cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

while(@fields = $cursor->fetchrow) {
  my $cols=$cursor->{NUM_OF_FIELDS};
  $theRunName = '';
  $theEventCount = 0;
  for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
    my $fname=$cursor->{NAME}->[$i];
    if ( $fname eq 'runname' ) {$theRunName = $fvalue}
    if ( $fname eq 'events' ) {$theEventCount = $fvalue}
  }
  if ( $theEventCount > 0 ) {$runNames{$theRunName} = $theEventCount}
}

foreach $dir ( sort keys %dataDirs ) {
    $host = `hostname`;
    chomp $host;
    if ( $host ne $dataDirs{$dir} ) {
        print "Not processing $dir; host $host not ".$dataDirs{$dir}."\n" if $debugOn;
        next;
    }
    opendir(DIR, $dir);
    while (defined ($df = readdir DIR)) {
        if ( $df ne "." && $df ne ".." ) {
            # something seems to be there
            if ( $df =~ m/\.daq$/ ) {
                if ( exists($badFiles{$df}) ) {next}  # flaky file
                # Process it if it hasn't been processed already
                if ( exists($runNames{$df}) ) {
                    print "Run file $df already processed: ".$runNames{$df}." events\n" if $debugOn;
                } else {
                    print "Scanning $dir/$df...";
                    $sys = $ENV{STAR_SYS};
                    `/star/u2d/wenaus/bin/$sys/daqscan $dir/$df`;
                    print "done\n";
                }
            }
        }
    }
    close DIR;
}
