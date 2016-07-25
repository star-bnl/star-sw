#! /usr/bin/env perl
use File::Basename;
use Cwd;
#print "ARGV $#ARGV: @ARGV\n";
if ($#ARGV != 0) {
  print "Usage: $0 file_name_with_list_of_files_on_hpss\n";
  exit;
}
my @list = ();
my $line;
open (In,"$ARGV[0]") or die "Cannot open $ARGV[1]\n";
while ($line = <In>) {
  chomp($line);
  push @list, $line;
}
close (In);
print "Request for $#list files\n";
my $pwd = cwd();
foreach my $file (@list) {
  print "file = $file\n";
  my $dir = File::Basename::dirname($file);
  my $lfile = File::Basename::basename($file);
  my $ldir = $dir;
  $ldir =~ s|\/home\/starsink\/raw\/daq\/||; print "dir = $dir ldir = $ldir\n";
  my $fullpath = $ldir . "/" . $lfile;
  print "$file => $fullpath\n";
  if (! -d $ldir) {`mkdir -p $ldir`;}
  if (! -r $fullpath) {
    print "get $file\n";
    chdir($ldir);
    `hsi get $file`;
    chdir($pwd);
  } else {
      print "$fullpath has been read\n";
    }
    $no++;
#    if ($no > 50) {last;}
}

