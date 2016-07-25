#! /usr/bin/env perl
my @files = qw(Y2001_07_01.root  Y2001_09_11.root  Y2001_09_24.root  Y2001_12_05.root);
my @histos = qw( SecRow3 SecRow3C );
foreach my $rootfile (@files) {
  my $Cmd = "root.exe -q -b " . $rootfile . " '/afs/rhic/star/users/fisyak/.dev/dEdxFit.C\(\"";
  foreach my $hist (@histos) {
    my $cmd = $Cmd;
    $cmd .=  $hist . "\")'";
    print "$cmd\n";
    my $flag = `$cmd`;
  }
}  
