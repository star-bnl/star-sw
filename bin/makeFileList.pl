#! /usr/bin/env perl
    use File::Basename;
my $FileIn = "daq.list";
open (In, $FileIn) or die "Can't open $FileIn";
my $line;
while ($line = <In>) {
  my $path = $line;
  chomp($path);
  my $file =  File::Basename::basename($path,".daq");
  for (my $i = 1; $i <= 125000; $i += 25000) {
    my $j = $i + 24999;
    my $out = $file;
    if ($i > 1) { $out .= "_" . $i;}
    $out .= ".event.root";
#    print "Check $out\n";
    my $hout = "./HOLD/" . $out;
    if (-r $hout) {
#      print "<-- <input URL=\"file:$path;$i;$j\" /> -->\n";
      last;
    }
    if (-r $out) {
#      print "<-- <input URL=\"file:$path;$i;$j\" /> -->\n";
    } else {
#      print "    <input URL=\"file:$path;$i;$j\" />\n";
      print "string:$path:$i:$j\n";
    }
  }
  
}
