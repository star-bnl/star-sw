#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @TestList = `root.exe -q -b bfc.C | awk -F\: '{print $3}' | egrep test`;
my $makeList = 0;
my $NoEvents = 100;
my $pwd = cwd();
my $extra = ",picoWrite";
my $noMC = 0;
if ($pwd =~ /StiCA/) {$extra .= ",StiCA";}
if ($pwd =~ /KF/) {$extra .= ",KFVertex";}
if ($pwd =~ /x8664/) {$noMC = 1;} #print "noMC = $noMC\n";
if ($makeList) {
  foreach my $line (@TestList) {
    $line =~ s/ //g;
    if ($line =~ /AgML/) { next; }
    if ($noMC and $line =~ /MC/) {next;}
#    print "$line";
    my ($dum,$dum,$test,$dum,$dum,$dum,$dum,$dum,$file) = split(":",$line);
    my $dir = $test;
    if ($dir !~ /test/  ) {next;}
    chomp($file);
    my $f = File::Basename::basename($file);
    if ($f eq "./") { next;}
    #  print "$test => $file => $f\n";
    $f =~ s/\.daq//;
    $f =~ s/\.fzd//;
    $f =~ s/\.fz//;
    $dir =~ s/test_//;
    $dir =~ s/test\.//;
    $dir =~ s/ITTF//;
    $dir =~ s/ittf//;
    $dir =~ s/simutrs/trs/;
    $dir =~ s/\.\././;
    if ($dir =~ /\.MC\./) {$dir =~ s/\.MC\./\./; $dir = "MC." . $dir;}
#    print "dir = $dir\n";
    my @words = split(/\./,$dir);
    #  for (my $i = 0; $i <= $#words; $i++) { 
    #    print "\t$words[$i]";
    #  }
    #  print "\n";
    my $DIR = $words[$#words] . "/";
    $DIR =~ s/y/year_/;
    for (my $i = 0; $i < $#words; $i++) {
      if ($i != 0) {$DIR .= ".";}
      $DIR .= $words[$i];
    }
    print "string:$test$extra:$DIR:$f:$NoEvents\n";
  }
} else {
  my $file = "/star/u/fisyak/bin/Nightlies.list";
  open(In, $file) or die "Can't open $file";
  while ( my $it = <In>) {
    if ($noMC and $it =~ /MC/) {next;}
    my ($string,$test,$DIR,$f,$NoEvents) = split(":",$it);
    if ($string ne 'string') {next;}
#    my @words = split(":",$it);
#    my $log = $words[2] . "/" . $words[3] . ".log";#  print "$log\n";
    my $log = $DIR . "/" . $f . ".log"; 
    if (-r $log) {next;}
    print "$string:$test$extra:$DIR:$f:$NoEvents";
  }
}
