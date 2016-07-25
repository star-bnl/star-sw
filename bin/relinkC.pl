#!/usr/bin/env perl
use File::Basename;
use FileHandle;
#my @lines = `root.exe -q -b GeometryTags.C`;
#my @vers = ();
#foreach my $line (@lines) {
#  next if $line !~ /^Geometry/;
##  print "$line";
#  next if $line =~ /simpletpc/;
#  my @words = split ' ',$line;
#  my $version = $words[5];
##  print "version = $version\n";
#  foreach my $v (@vers) {
#    next if ($v ne $version);
#    goto LAST;
#  }
#  push @vers, $version;
# LAST:
#}
#print "vers $#vers: @vers\n";
#die;
my @vers = qw(year2000 year_2b year2001 year2002 year_2a
	      year2003 y2003x y2003a y2003b y2003c
	      y2004 y2004x y2004y y2004a y2004b y2004c
	      y2004d y2005x y2005 y2005b y2005c y2005d y2005e y2005f y2005g
	      y2006 y2006a y2006b y2006c y2006g
	      y2007 y2007a y2007g y2008
	      dev2005 complete upgr01 upgr02 upgr03 upgr04 upgr05 upgr06 upgr07
	      upgr08 upgr09 upgr10 upgr11 upgr12 upgr13 upgr14 upgr15 upgr16 upgr17 upgr20 upgr21);
my @list = `ls *.C`;
#print "list $#list: @list\n";
my %Hash = ();
foreach my $file (@list) {
  chomp($file);
  my ($name,$version,$dum) = split /\./, $file;
#  print "$file => $name,$version,$dum\n";
  if ($version eq 'h') {
    $version = $name;
    $name = ' ';
  }
  if ($Hash{$name}) { $Hash{$name} .= "|" . $version;}
  else              { $Hash{$name}  =       $version;}
#  print "$name => $Hash{$name}\n";
}
foreach my $key (sort keys %Hash) {
  my $order = "";
#  print "$key => $Hash{$key}\n";
  my @versions = split '\|', $Hash{$key};
  foreach my $v (@vers) {
    foreach my $V (@versions) {
      if ($v eq $V) {
	if ($order) { $order .= "|" . $v;}
	else        { $order  =       $v;}
	last;
      }
    }
  }
  $Hash{$key} = $order;
#  print "$key => $Hash{$key}\n";
}
my $status = 0;
my $cmd = "";
foreach my $key (sort keys %Hash) {
  my $name = $key;
#  if ($name eq '^ ') {$name = "";}
  if ($name eq '' or $name eq '^ ') {$next;}
  my @versions = split '\|', $Hash{$key};
  print "|$key| versions $#versions: @versions\n";
  if ($key eq ' '  or $key eq 'Material' or $key eq 'Media') {next;}
  my $config = 1;
  my $OutFile = $name . ".C";
  open (Out, ">$OutFile") or die "Can't open $OutFile";
  my $line = "#ifdef " . $name . "Config\n";
  print Out $line;
  for (my $i = 0; $i <= $#versions; $i++) {
    my $fo  = $name . "." .  $versions[$i] . ".C";
    if ( -l $fo) {next;}
#    my $link = readlink $fno;
#    print "$fo => link = $link\n";
#    if ($link) {next;}
    my $fn1 = $name . "Config==" . $config;
    print "$fo => $fn1 =======================================\n";
#    rename $fo, $fn1; 
#    symlink $fn1, $fo; 
#    $cmd = "mv $fo $fn1"; print "$cmd\n";
#    $status = system($cmd);
#    if ($status) {die "$cmd does not work";}
    rename $fo, $fn1;
#    $cmd = "ln -s  $fn1 $fo"; print "$cmd\n";
#    $status = system();
#    if ($status) {die "$cmd does not work status = $status $!";}
    symlink $fn1, $fo;
    if (! -r $fn1) {die "Can't open $fn1";}
    print "i = $i $#versions => config = $config <<<<<<<<<<<<<<<<<<<<<<\n";
    for (my $j = $i+1; $j <= $#versions; $j++) {
      my $fn2  = $name . "." .  $versions[$j] . ".C";
#      print "j = $j => $fn2\n";
      if (! -r $fn2) {die "Can't open $fn2";}
      my $cmd = "diff -q $fn1 $fn2";
      $status = system($cmd);
      print "$cmd => $status \n";
      if ($status == 0) {
	my $fn2BAK = $fn2 . ".BAK";
#	$cmd = "mv $fn2 $fn2BAK";
#	$status = system($cmd);
#	if ($status) {die "$cmd does not work";}
	print "rename $fn2, $fn2.BAK\n";
	rename $fn2, $fn2 . ".BAK";
	print "symlink $fn1, $fn2\n";
	symlink $fn1, $fn2;
#	$cmd = "ln -s  $fn1 $fn2";
#	$status = system($cmd);
#	if ($status) {die "$cmd does not work";}
	print "$fn2 => $fn1\n";
      }
    }
#    die;
    my $lines = "#if " . $name . "Config==" . $config ."\n";
    $lines .=    "#include \"" . $fn1 . "\"\n";
    $lines .=    "#endif\n";
#    print $lines;
    print Out $lines;
    $config++;
  }
  print Out "#endif\n";
  close (Out); 
}
