#!/usr/bin/env perl
# foreach f (`dir -lt *B.log | tail -1300 | awk '{print $9}' | xargs grep -l 'QAInfo:Run is finished at Date'`) 
# foreach f (`dir -lt *B.log | tail -1300 | awk '{print $9}' | xargs grep -l 'StCloseFileOnTerminate::Notify : Terminating'`)
# set b = `basename ${f} B.log`; mv ${b}* Done/
# end
use File::Basename;
use Cwd;
my $daq = "";
foreach my $log (@ARGV) {
  open(IN,$log) or die "Can't open $log";
  my $line;
  while ($line = <IN>) {
    if ($line =~ /Processing bfc/) {
      my @words = split('"',$line);
      $daq = $words[3];# print "daq = $daq\n";
    }
    next if $line !~ 'QAInfo:Run is finished at Date/Time';
    print "$daq\n";
  }
  close(IN);
}
