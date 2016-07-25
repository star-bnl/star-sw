#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $date = 20130215;
my $time =   202757;
my @DistortionSet = qw(Corr4 Corr3 OBmap2D OTwist OClock Opr13 OIFC OShortR OBmap OSectorAlign);
foreach my $field (qw(FF RF)) {
  if ($field eq 'FF') {$date = 20130215; $time =   202757;}
  else                {$date = 20130329; $time =   105728;}
  foreach my $corr (@DistortionSet) {
    my $rootfile = $corr . $field . ".root";
    if (-r $rootfile) {next;}
    my $cmd = "root.exe -q -b 'CheckDistortion.C(\"" . $corr . "\"," . $date . "," . $time . ",\"" . $rootfile . "\")' >& " . $corr . $field . ".log";
    print "cmd $cmd\n";
    my $flag = system($cmd);
    if ($flag) {print "flag = $flag\n";}
  }
}
