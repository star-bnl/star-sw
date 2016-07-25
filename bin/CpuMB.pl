#! /usr/bin/env perl
my $line;
my $CERNCPU  = 497.;
my $nfiles = 0;
my $TimePerEventAv = 0;
my $TimePerEventRMS = 0;
my $CNUPerMBAv = 0;
my $CNUPerMBRMS = 0;
my $TotalNoEvents = 0;
foreach my $input_file (@ARGV) {
  open(In,$input_file) or die "Can't open $input_file";
  my $time;
  my $event = 0;
  my $cpu = 0;
  my $Size  = 0;
  my $file;
  while ($line = <In>) {
    if ($line =~ /CPU time   :/ && $line =~ /sec\./) {
#      print $line;
      my @words = split ' ', $line;# my $i = 0; foreach my $w (@words) {print "$i $w\n"; $i++}
      $time = $words[3]; #print "time = $time\n";
    }
    elsif ($line =~ /input file = /)  {
#      print $line;
      my @words = split ' ', $line;# my $i = 0; foreach my $w (@words) {print "$i $w\n"; $i++}
      $file = $words[3]; #print "file = $file\n";
#      my ($dev,$ino,$mode,$nlink,$uid,$gid) = lstat($file);
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, 
	  $mtim, $ctime, $blksize,$blocks) = stat($file);
#      print " dev, ino, mode, nlink, uid, gid, dev,  size,  atime,  mtim,  ctime,  blksize,  blocks\n";
#      print "$dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize, $blocks\n";
# dev, ino, mode, nlink, uid,  gid, dev,     size,      atime,       mtim,      ctime, blksize,  blocks
#   0, 737,33188,     1,5691,31152,   0, 51062400, 1050163697, 1046608193, 1046608193,   32768,   99748
      $Size = $size;
#      print "Size = $Size\n";
    }
    elsif ($line =~ /CPU clock speed is/) {
#      print $line;
      my @words = split ' ', $line;# my $i = 0; foreach my $w (@words) {print "$i $w\n"; $i++}
      $cpu = $words[5]; #print "cpu = $cpu\n";
    }
    elsif ($line =~ /ARecon/) {$event++;}
  }
  if (! $cpu or ! $event) {next;}
#  print "Events = $event\n";
  my $CNUtime  = $time*$cpu/$CERNCPU;
  my $TimePerEvent = int $time/$event;
  my $CNUPerEvent  = int  $CNUtime/$event;
  my $TimePerMB    = $time/$Size; $TimePerMB *= 1024;$TimePerMB *= 1024;
  $TimePerMB       = int $TimePerMB;
  my $CNUPerMB     = $CNUtime/$Size; $CNUPerMB *= 1024;$CNUPerMB *= 1024;
  $CNUPerMB        = int $CNUPerMB;
  $TotalNoEvents    += $event;
  print "$TimePerEvent(sec/Event)\t$TimePerMB(sec/MB)\t$CNUPerEvent(CNUsec/Event)";
  print "\t$CNUPerMB(CNUsec/MB)\tEvents = $event\tfile = $file\n";
  $nfiles++;
  $CNUPerEventAv += $CNUPerEvent; $CNUPerEventRMS += $CNUPerEvent*$CNUPerEvent;
  $CNUPerMBAv += $CNUPerMB; $CNUPerMBRMS += $CNUPerMB*$CNUPerMB;
}
if ($nfiles > 2) {
  $CNUPerEventAv /= $nfiles;
  $CNUPerEventRMS /= $nfiles; 
  $CNUPerEventRMS = $CNUPerEventRMS - $CNUPerEventAv*$CNUPerEventAv;
  $CNUPerEventRMS = sqrt($CNUPerEventRMS);
  $CNUPerMBAv /= $nfiles;
  $CNUPerMBRMS /= $nfiles; 
  $CNUPerMBRMS = $CNUPerMBRMS - $CNUPerMBAv*$CNUPerMBAv;
  $CNUPerMBRMS = sqrt($CNUPerMBRMS);
  print " $CNUPerEventAv +/- $CNUPerEventRMS (CNUsec/Event) \t $CNUPerMBAv +/- $CNUPerMBRMS (CNUsec/MB)\n";
}
print "Total no. of events processed $TotalNoEvents\n";
