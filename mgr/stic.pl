#!/usr/bin/env perl
use File::Basename;
use Cwd;
use File::Copy;

my $target = shift;
my @pars = ();
while (my $par = shift) {
  if ($par =~ /ROOT|ObjectSpace/) {next;} 
  push @pars, $par;
}
my $file = basename($target); 
my $dir  = dirname($target);   
my $Dir  = cwd();
my $idl  = pop @pars;

# check target path
if ($idl !~ /^\//) { $idl = $Dir . "/" . $idl;}
if (! -d $dir    && !mkdir ($dir,    0755)) {
    die "$0:Can't create directory $dir\n";
}

# create temp dir
my $tmpdir = "/tmp/" . $$;
if (! -d $tmpdir && !mkdir ($tmpdir, 0755)) {
    die "$0:Can't create directory $tmpdir\n";
}

# exec command
my $com = "cd $tmpdir && stic @pars $idl";
my $exec = `$com`; if ($?) { exit 2; }

# copy file and delete temp directory
my $sfile = $tmpdir . "/" . $file;
if (-f $sfile) {copy ($sfile,$target);}
`rm -rf $tmpdir`; 
if ($?) { exit 2; }
exit 0;

