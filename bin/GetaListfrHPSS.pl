#!/usr/bin/perl -w
foreach my $item (@ARGV) {
  my $i1 = $item;
  my $i2 = $i1;
  if ($item =~ /-/) { ($i1,$i2) = split '-', $item;}
  print "i1 = $i1, i2 = $i2\n";
  for(my $i=$i1; $i<=$i2; $i++) {
    my $j = $i;
       if ($i <   9) {$j = "000" . $i;}
    elsif ($i <  99) {$j = "00"  . $i;}
    elsif ($i < 999) {$j = "0"   . $i;}
    my $filename = "dc1.002001.lumi10.0".$j.".hlt.pythia_jet_25.zebra";
    print "$filename\n";
    next if -r $filename;
    my $findfile = `magda_findfile $filename --local`; 
    print "$findfile";
    if( $findfile =~ m/^LFN:\/\/atlas.org\/$filename/g ) {
      print "magda_getfile $filename\n";
      my $magdagetfile = `magda_getfile $filename --local`;
      print "$magdagetfile\n";
    }
  }
}
print "Done\n";
exit 0;
