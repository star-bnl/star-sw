#! /usr/bin/env perl
    use File::Basename;
# laser
# FieldOff 
#my $gcmd = "get_file_list.pl -delim \"#\" -keys 'runnumber,path,basename,events' -cond 'filetype=online_daq,filename~st_phy,tpc=1,svt=1,ssd=1,runnumber[] 8105000-8113999,path~2007,events>100' -limit 1000";
my $gcmd = "get_file_list.pl -delim \"#\" -keys 'runnumber,path,basename,events' -cond 'filetype=online_daq,filename~st_phy,tpc=1,svt=1,ssd=1,runnumber[] 8105000-8105999,path~2007,events>100' -limit 0";
#  my $gcmd = "get_file_list.pl -delim \"#\" -keys 'path,basename,events' -cond 'filetype=online_daq,filename~laser,tpc=1,path~2012,events>100' -limit 0";
print "$gcmd\n";
my @list = `$gcmd`; #print @list;
#  next;
my $cmd = "";
my %file2dir = ();
foreach my $line (@list) {
  #    print "$line";
  my ($run,$dir,$file,$events) = split '#', $line;
  my $ldir = $dir;
  my $ffile = $dir . "/" . $file . ".daq";# print "ffile = $ffile\n";
  $ldir =~ s#\/home\/starsink\/raw#\/star\/data10\/VFtest#; print "$dir => $ldir\n";
  if (! -r $ldir) {`mkdir -p $ldir`; `chmod g+w $ldir`;}
  my $lfile = $ldir . "/" . $file . ".daq";
  if (-r $lfile) {next;}
  if (! $file2dir{$dir}) {$file2dir{$dir} = $ldir . '|';}
  $file2dir{$dir} .= " " . $ffile;
}
#  next;
foreach my $key( sort keys %file2dir) {
  my $df = `df -h /star/data10 | tail -1 | awk '{print $3}'`;
  if ($df =~ 'G$') {
    $df =~ s/G//;
    if ($df < 500) {last;}
    print "still $df GB on disk\n";
  }
  print "$key => $file2dir{$key}\n";
  my ($ld,$ll) = split '\|', $file2dir{$key};  print "$ld => $ll\n";
  if (! $ll) {next;}
  if (! -r $ld) {`mkdir -p $ld`; `chmod g+w $ld`;}
  chdir($ld);
  #    my $cmd = "hsi get " . $ffile . ":" . $lfile . " << EOF
  my @fls = split ' ',$ll; print "fls = $#fls\n";
  my $NP = int ($#fls)/20; 
  if ($#fls > 20*$N) {$NP++;}
  for (my $p = 0; $p < $NP; $p++) {
    my $j1 = 20*$p;
    my $j2 = $j1 + 20;
    if ($j2 > $#fls) {$j2 = $#fls;}
    $cmd = "hsi \"prompt; mget ";
    my $g = 0;
    for (my $j = $j1; $j < $j2; $j++) {
      $cmd .= " " . $fls[$j];
      $g++;
    }
    $cmd .= "\"\n";
    if ($g > 0) {
      print "$cmd"; 
      my $flag = `$cmd`; 
      if (! $flag) {last;}
    }
  }
}
