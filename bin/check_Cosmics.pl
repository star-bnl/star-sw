#! /usr/bin/env perl
    use File::Basename;
# get_file_list.pl -delim "#" -all -keys 'magscale,path,basename,events' -cond 'filetype=online_daq,trgsetupname=CosmicLocalClock' -limit 0 | tee daq_full.list 
# grep 2011 daq_full.list | grep -v adc | grep -v laser | grep tofcosmic > tofcosmic_2011.list
#my $tofcosmic_2011 = "/star/institutions/bnl/fisyak/Tpc/Alignment/tofcosmic_2011.list";
my $tofcosmic_2011 = "/star/institutions/bnl/fisyak/Tpc/Alignment/daq_full.list";
print "Input list: $tofcosmic_2011\n";
open(In,$tofcosmic_2011) or die "Can't open tofcosmic_2011";
my $line;
my %Hash = ();
my $Total = 0;
#my $FieldChoise = "FullField";
my $FieldChoise = "FieldOff";
#my $FieldChoise = "ReversedFullField";
while ($line = <In>) {
#  print "$line";
  chop($line);
  my ($Field,$dir,$key,$events) = split ("#",$line);
  if ($key =~ /adc/) {next;};
  if ($dir !~ /\/2011\//) {next;}
#  if ($Field ne 'ReversedFullField') {next;}
#  if ($Field ne 'FieldOff') {next;}
  if ($Field ne $FieldChoise) {next;}
  if ($events < 10000) {next;}
#  print "$Field,$dir,$key,$events\n";
  $Total  += $events;
  my $Dir = $dir;
  $Dir =~ s#\/home\/starsink\/raw#\/star\/data03#;
  my $File = $Dir . "/" . $key . ".daq";
  if (-r $File) {
    $Hash{$key} = $line . "#1";
  } else {
    $Hash{$key} = $line . "#0";
    print "$key => $Hash{$key}\n";
  }
}
print "Total no. of events: $Total\n";
#foreach my $dir (@list) { 
#  my $ldir = $dir;
##  $ldir =~ s#\/home\/starsink\/raw#\/star\/data03#; print "$dir => $ldir\n";
##  $ldir =~ s#\/home\/starsink\/raw#\/star\/data06\/TPCCalib#; print "$dir => $ldir\n";
#  $ldir =~ s#\/home\/starsink\/raw#\/star\/institutions\/bnl\/fisyak#; print "$dir => $ldir\n";
##  $ldir =~ s#\/home\/starsink\/raw#\/star\/data07\/calib\/fisyak#; print "$dir => $ldir\n";
#  if (! -r $ldir) {`mkdir -p $ldir`; `chmod g+w $ldir`;}
#  chdir($ldir);
#  my $cmd = "hsi mget " . $dir . "/st_tofcosmic_12*.daq << EOF
#EOF
#";
#  print "$cmd\n";
#  my $flag = `$cmd`;
##  last;
#}
