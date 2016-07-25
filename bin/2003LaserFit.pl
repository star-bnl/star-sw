#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @fileList = glob "/star/institutions/bnl/fisyak/Tpc/Alignment/Laser/2003/st_laser_4*.ClnoW.root";
#my @fileList = glob "/star/institutions/bnl/fisyak/Tpc/Alignment/Laser/2012/st_laser_13*.Cl.root";
foreach my $file (@fileList) {
  for (my $i = 1; $i <= 24; $i++) {
    my $f = File::Basename::basename($file);
    $f =~ s/\.root//;
    $f .= ".s" . $i . ".Fit8.root";
#    print "File : $file $i => $f\n";
    if (-r $f) {next;}
    print "string:$file:$i\n";
  }
}
