#!/usr/bin/env perl
use File::Basename;
use Cwd;
my @FileList = glob "/star/data90/reco/*/*/*/reco_St*/*event.root";
my %Dirs = {};
my @Prods = ();
foreach my $f (@FileList) {
  my $dir = File::Basename::dirname($f);
  my $tag = $dir;
  $tag =~ s#/star/data90/reco/##; # print "tag : $tag\n";
  my $vers = File::Basename::basename($tag); $vers =~ s#reco_##; print "vers : $vers\n";
  my $prod = File::Basename::dirname($tag);                      print "prod : $prod\n";
  my @hash = ($prod =>{tag => $tag, vers => $vers, dir => $dir});
  push @Prods, @hash;
}
print "Prod @Prod\n";
