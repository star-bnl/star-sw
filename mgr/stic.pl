#! /usr/local/bin/perl
#use File::Basename;
use File::Copy;
#print "stic @ARGV\n";
my $dir = shift;
my $stem = shift;
my @pars = ();
while (my $par = shift) {
  if ($par =~ /Root|ROOT|ObjectSpace/) {next;} 
  push @pars, $par;
}
my $tmpdir = "/tmp/" . $$;# print "tmpdir = $tmpdir \n";
if (! -d $dir    && !mkdir ($dir,    0755)) {die "$0:Can't create directory $dir\n";};
if (! -d $tmpdir && !mkdir ($tmpdir, 0755)) {die "$0:Can't create directory $tmpdir\n";};
my $com = "cd $tmpdir && stic @pars";# print "=========================\n$com\n";
my $exec = `$com`; if ($exec) { exit $exec; }
my @files = ($stem . ".h",$stem . ".inc","St_" . $stem . "_Module.h","St_" . $stem . "_Module.cxx");
foreach my $file (@files){
  my $sfile = $tmpdir . "/" . $file;
  my $dfile = $dir    . "/" . $file;
  if (-f $sfile) {copy ($sfile,$dfile);}
}
exit `rm -rf $tmpdir`;
