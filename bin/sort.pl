#!/usr/bin/env perl
my $line;
while ($line =  <>) {
  next if $line !~ /^QA :INFO/;
#  print "$line;
  chomp($line);
#QA :INFO  -  1046: test.RC.2005.CuCu22_MinBias   :            :            :P2005,tofDat,MakeEvent,ssddat,spt,SsdIt,SvtIt,pmdRaw,OShortR,OSpaceZ2,ITTF,Sti:::/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq
#words[1] = INFO  -  1046
#words[2] =  test.RC.2005.CuCu22_MinBias   
#words[3] =             
#words[4] =             
#words[5] = P2005,tofDat,MakeEvent,ssddat,spt,SsdIt,SvtIt,pmdRaw,OShortR,OSpaceZ2,ITTF,Sti
#words[6] = 
#words[7] = 
  my @words = split(':', $line);
  for (my $i = 2; $i <= $#words; $i++) {
    $words[$i] =~ s/ //g;
#    print "words[$i] = |$words[$i]|\n";
    if ($i == 5) {
      my $Chain = $words[5];
      my $chain = lc($Chain);
      my @w = split(',',$chain);
      my @wd = sort @w;
      $words[$i] = join(',',@wd);
#      print "words[$i] = |$words[$i]|\n";
    }
  }
  print "  {";
  for (my $i = 2; $i <= $#words; $i++) {
    print "\"$words[$i]\",\t";
  }
  print ",kFALSE},\n";
}
