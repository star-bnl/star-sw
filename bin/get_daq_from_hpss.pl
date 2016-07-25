#! /usr/bin/env perl
    use File::Basename;
if ($#ARGV < 0) {
  print "Usage: $0 File_list\n";
  exit 0;
}
open(In, $ARGV[0]) or die "Can't open $ARGV[0]";
while (my $line = <In>) {
  chomp($line);
  my $file = $line; print "$line\n";
  my $dir = File::Basename::dirname($file);
  my $f   = File::Basename::basename($file);
  my $ldir = $dir;
#  $ldir =~ s#\/home\/starsink\/raw#\/star\/data03#; print "$dir => $ldir\n";
#  $ldir =~ s#\/home\/starsink\/raw#\/star\/data06\/TPCCalib#; print "$dir => $ldir\n";
  $ldir =~ s#\/home\/starsink\/raw#\/star\/institutions\/bnl\/fisyak#; print "$dir => $ldir\n";
#  $ldir =~ s#\/home\/starsink\/raw#\/star\/data07\/calib\/fisyak#; print "$dir => $ldir\n";
  my $lf = $ldir ."/". $f; print "local file = $lf\n";
  if (-r $lf) {next;}
  if (! -r $ldir) {`mkdir -p $ldir`; `chmod g+w $ldir`;}
  chdir($ldir);
#  my $cmd = "hsi mget " . $dir . "/st_tofcosmic_12*.daq << EOF
#  my $cmd = "hsi mget " . $dir . "/st_laser_12*.daq << EOF
  my $cmd = "hsi mget " . $file . " << EOF
EOF
  "; 
  print "$cmd\n";  
  my $flag = `$cmd`;  
#  last; 
}   
