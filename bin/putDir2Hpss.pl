#! /usr/bin/env perl
use File::Basename;
use File::Find;
use File::Path;
my @search_files = ();
#________________________________________
sub wanted {
  #  print "wanted ",$_,"\n";
  my ($dev,$ino,$mode,$nlink,$uid,$gid) = lstat($_);
  if ( /^\.\#/) {return;}
  my $base = File::Basename::basename($_);
  if ($base !~ /^Event/) {return;}
  if (
      /^\w.*\.root$/) { 
    push @search_files,  $File::Find::name;
#    print " $File::Find::name => @search_files\n";
  }
}
my @dirs = @ARGV;
if ($#dirs < 0) {$dirs[0] = ".";}
&File::Find::find(\&wanted, @dirs);
print "@search_files\n";
foreach my $file (@search_files) {
  my @dirs = split '\/', $file; print "$file => $#dirs @dirs\n";
  my $cmd = "hsi \"cd SvtSsdAlignment;";
  my $n = $#dirs;
  for (my $i = 0; $i < $n; $i++) {
    $cmd .= "mkdir " . $dirs[$i] . "; cd " . $dirs[$i] .";";
  }
  $cmd .= "put " . $file . " : " . $dirs[$n] . ";\"";
  print "cmd = $cmd\n";
  $flag = system($cmd);
  print "flag = $flag\n";
  last if $flag;
  unlink ($file);
}
