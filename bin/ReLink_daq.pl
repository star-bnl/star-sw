#!/usr/bin/env perl
 use File::Basename;
#while (my $line = <>) {
#  if ($line =~ /[^a-zA-Z_0-9 \t\n\r\f\(\)\{\}\]/\[\]\'\"]/) {print $line;}
#}
while (my $file = <>) {
  chop($file);
  my $daq = File::Basename::basename($file);
  if ($daq !~ /adc/) {next;}
  if ($daq !~ /^st_phys/) {next;}
  my @words = split("_",$file); # for (my $i = 0; $i <$#words; $i++) {print "$i\t$words[$i]\n";}
  my $run = $words[3];
  my $y = int( $run/1000000 );
  my $year = 1999 + $y;
  my $day = int( (($run - 1000000*$y)/1000));
  print "file = $file => run = $run, year = $year, day = $day\n";
  my $DIR = "/star/data03/daq/" . $year . "/" . $day . "/" . $run;
  my $lfile = $DIR . "/" . $daq;
  if (-r $lfile) {next;}
  if (! -d $DIR) {
    my $cdir = "mkdir -p $DIR";
    print "$cdir\n"; 
    my $flag = system($cdir);
    if ($flag) {die "Cam't execute $cdir";}
  }
  my $cmd = "ln -s $file $DIR/$daq";
  print "$cmd\n";
  my $f = system($cmd);
  if ($f) {die "Cam't execute $cdir";}
}
