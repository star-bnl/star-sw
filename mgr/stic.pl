#!/usr/bin/env perl
use File::Basename;
use Cwd;
use File::Copy;
#print "stic @ARGV\n";
my $target = shift;
my @pars = ();
while (my $par = shift) {
  if ($par =~ /ROOT|ObjectSpace/) {next;} 
  push @pars, $par;
}
my $file = basename($target); 
my $dir = dirname($target);   
my $Dir = cwd();
my $idl = pop @pars;
if ($idl !~ /^\//) {$idl = $Dir . "/" . $idl;}# print "Full name $idl\n";
my $tmpdir = "/tmp/" . $$;# print "tmpdir = $tmpdir \n";
if (! -d $dir    && !mkdir ($dir,    0755)) {die "$0:Can't create directory $dir\n";};
if (! -d $tmpdir && !mkdir ($tmpdir, 0755)) {die "$0:Can't create directory $tmpdir\n";};
my $com = "cd $tmpdir && stic @pars $idl";# print "=========================\n$com\n";
my $exec = `$com`; if ($?) { exit 2; }
my $sfile = $tmpdir . "/" . $file;# print "sfile $sfile\n";
if (-f $sfile) {copy ($sfile,$target);}
`rm -rf $tmpdir`; 
if ($?) { exit 2; }
exit 0;
