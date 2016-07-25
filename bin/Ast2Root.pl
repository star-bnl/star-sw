#!/usr/bin/env perl
my @lines = `root.exe -q -b GeometryTags.C`;
my @vers = ();
foreach my $line (@lines) {
  next if $line !~ /^Geometry/;
#  print "$line";
  next if $line =~ /simpletpc/;
  my @words = split ' ',$line;
#  my $version = $words[5];
  my $version = $words[2];
#  print "version = $version\n";
  foreach my $v (@vers) {
    next if ($v ne $version);
    goto LAST;
  }
  push @vers, $version;
 LAST:
}
foreach my $v (@vers) {
  my $CINT = $v . ".h";
  if (! -r $CINT) {
    my $cmd = "root4star -q -b 'Ast2Root.C(\"";
    $cmd .= $v;
    $cmd .= "\")'";
    $cmd .= " >& " . $v . ".log";
    print "$cmd\n"; system($cmd);
  }
}
