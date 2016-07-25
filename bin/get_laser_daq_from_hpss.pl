#! /usr/bin/env perl
    use File::Basename;
# laser
# FieldOff 
my @Fields = qw( ReversedFullField FullField FieldOff);
#my @Fields = qw( ReversedFullField FullField FieldOff);
foreach my $field (@Fields) {
  my $gcmd = "get_file_list.pl -delim \"#\" -keys 'path,basename,events' -cond 'magscale=" . $field . ",filetype=online_daq,filename~laser,tpc=1,path~2003,events>100' -limit 0";
#  my $gcmd = "get_file_list.pl -delim \"#\" -keys 'path,basename,events' -cond 'filetype=online_daq,filename~laser,tpc=1,path~2012,events>100' -limit 0";
  print "$gcmd\n";
  my @list = `$gcmd`;
  print @list;
#  next;
  my $cmd = "";
  my %file2dir = ();
  foreach my $line (@list) {
#    print "$line";
    my ($dir,$file,$events) = split '#', $line;
    my $ldir = $dir;
    my $ffile = $dir . "/" . $file . ".daq";# print "ffile = $ffile\n";
    $ldir =~ s#\/home\/starsink\/raw#\/star\/institutions\/bnl\/fisyak#; print "$dir => $ldir\n";
    if (! -r $ldir) {`mkdir -p $ldir`; `chmod g+w $ldir`;}
    my $lfile = $ldir . "/" . $file . ".daq";
    if (-r $lfile) {next;}
    if (! $file2dir{$dir}) {$file2dir{$dir} = $ldir . '|';}
    $file2dir{$dir} .= " " . $ffile;
  }
#  next;
  foreach my $key( sort keys %file2dir) {
    my $df = `df -h /star/institutions/bnl | tail -1 | awk '{print $3}'`;
    if ($df =~ 'G$') {
      $df =~ s/G//;
      if ($df < 100) {last;}
      print "still $df GB on disk\n";
    }
    print "$key => $file2dir{$key}\n";
    my ($ld,$ll) = split '\|', $file2dir{$key}; print "$ld => $ll\n";
    if (! $ll) {next;}
    if (! -r $ld) {`mkdir -p $ld`; `chmod g+w $ld`;}
    chdir($ld);
#    my $cmd = "hsi get " . $ffile . ":" . $lfile . " << EOF
    $cmd = "hsi \"prompt; mget " . $ll ."\"\n";
    print "$cmd"; 
    my $flag = `$cmd`; 
  }
}
