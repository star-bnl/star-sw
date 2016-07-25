#! /usr/bin/env perl
use File::Basename;
use Cwd;

#my $vers = "B"; # default geometry
#my $vers = "G"; # add walls           <==========
#my $vers = "U.1ks", # increase time cut to 1e3 sec <========
#my $vers = "W"; # enlarged tcut
#my $vers = "V"; # new concrete and new counter postion
#my $vers = "U"; # extern histograms
#my $vers = "Za"; # add PMMA and BEMC electronics
#my $vers = "Zb"; # use TRandom3
#my $vers = "Zc"; # use TRandom3(0 ) <=========
#my $vers = "Zd"; # remove PMMA from MAGP
#my $vers = "Ze"; # add PMMA to MAGP, old definition of concrete
#if pp500
#my $vers = "Z"; # modified Magp geometry
#elseif pp200
my $vers = "pp200";
#my $pwd = cwd();
#my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
#$dir =~ s/^[a-zA-Z]*//;# print "dir = $dir\n";
#my $rung1 = $dir + 0;
#my $rung2 = $rung1 + 499;
my $rung1 = 1;
my $rung2 = 100;
for (my $rung = $rung1; $rung <= $rung2; $rung++) {
  my $file = "flux" . $vers . $rung . ".root";
#  print "file = $file\n";
  if (! -r $file) {
    print "string:$vers:$rung:100\n";
  }
 # last;
}
#endif
