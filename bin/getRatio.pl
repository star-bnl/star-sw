#!/usr/bin/env perl
if ($#ARGV < 0) {
  print "Usage $0 log_file\n";
}
foreach my $File (@ARGV) {
  open(In,"$File") or die "Can't open $File";
  my $line;
  my $MEAN = 0;
  my $RMS =  0;
  my $NoEntries = 0;
  while ($line = <In>) {
    next if $line !~ / TPC hits:/;
    #  print $line;
    my @words = split ' ',$line;
    #  my $i = 0;
    #  foreach my $w (@words) {
    #    print "$i $w\n";
    #    $i++;
    #  }
    my $total = $words[6]; $total =~ s/://;
    my $used  = $words[12];
    my $ratio = $used/$total;
    #  print "total = $total used = $used ratio = $ratio\n";
    if ($total > 0) {
      $NoEntries++;
      $MEAN += $ratio;
      $RMS  += $ratio*$ratio;
    }
    #  die;
  }
  close(In);
  $MEAN = $MEAN/$NoEntries; 
  $RMS  = $RMS/$NoEntries;
  #print "Ratio = $MEAN +/- $RMS\n";
  my $sigma = sqrt($RMS - $MEAN*$MEAN);
  #print "Ratio = $MEAN +/- $sigma\n";
  printf("%s \tRatio = %8.3f +/- %8.3f %%\n",$File,100*$MEAN,100*$sigma);
}
