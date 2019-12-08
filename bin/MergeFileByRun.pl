#!/usr/bin/env perl 

#my $input = "/star/u/fisyak/DB/Run19.list";
#open(In,$input) or die "Can't open $input";
my $line;
my $glob = "*.root";
my @Files = glob $glob;
my %Runs = ();
foreach my $file (@Files) {
  my $r = $file;
  $r =~ s/hlt_//;
  $r =~ s/st_physics_//;
  $r =~ s/adc_//;
  my ($run) = split '_', $r;
  if (! $Runs{$run}->{NoFiles}) {
    $Runs{$run}->{NoFiles} = 1;
    $Runs{$run}->{list} = $file;
  } else {
    $Runs{$run}->{NoFiles}++;
    $Runs{$run}->{list} .= " " . $file;
  }
}
foreach my $run (sort keys %Runs) {
#  print "run = $run, No.files = $Runs{$run}->{NoFiles}, List = $Runs{$run}->{list}\n";
  my $runf = $run . ".root";
  my $cmd;
  if ($Runs{$run}->{NoFiles} == 1) {
    $cmd = "mv $Runs{$run}->{list} $runf";
  } else {
    $cmd = "hadd $runf $Runs{$run}->{list}";
  }
  print "$cmd\n";
  $flag = system($cmd); 
  if ($flag) {last;}
}
