#! /opt/star/bin/perl
use File::Basename;
use Cwd;
use File::Copy;
#print "stic @ARGV\n";
my $target = shift;
my @pars = ();
while (my $par = shift) {
  if ($par =~ /Root|ROOT|ObjectSpace/) {next;} 
  push @pars, $par;
}
my $file = basename($target); #print "file = $file\n";
my $dir = dirname($target);   #print "dir = $dir\n";
my $Dir = cwd();
my $idl = pop @pars;# print "get $idl\n";
if ($idl !~ /^\//) {$idl = $Dir . "/" . $idl;}# print "Full name $idl\n";
my $tmpdir = "/tmp/" . $$;# print "tmpdir = $tmpdir \n";
if (! -d $dir    && !mkdir ($dir,    0755)) {die "$0:Can't create directory $dir\n";};
if (! -d $tmpdir && !mkdir ($tmpdir, 0755)) {die "$0:Can't create directory $tmpdir\n";};
my $com = "cd $tmpdir && stic @pars $idl";# print "=========================\n$com\n";
my $exec = `$com`; if ($exec) { exit $exec; }
my $sfile = $tmpdir . "/" . $file;# print "sfile $sfile\n";
if (-f $sfile) {copy ($sfile,$target);}
exit `rm -rf $tmpdir`;
