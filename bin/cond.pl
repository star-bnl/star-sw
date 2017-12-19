#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $debug = 0;
#my @list = ` condor_q -l -s fisyak | egrep '(GlobalJobId|NumCkpts_RAW|Iwd|ClusterId|RemoteUserCpu =)'`;
my @list = ` condor_q -global -l -s fisyak | egrep '(GlobalJobId|Iwd|RemoteUserCpu =|HoldReasonCode)'`;
#my @list = ` condor_q -l -s fisyak | egrep '(GlobalJobId|Iwd|CurrentHosts =|HoldReasonCode)'`;
#print "@list\n";
my $line = "";
my %Hash = ();
my %HashR = (); # RUN
my %HashH = (); # HOLD
my $GlobalJobId = "";
my $Iwd = "";
my $RemoteUserCpu = 0;
for (my $i = 0; $i < $#list; $i += 4) { 
  print "list:\n 0: $list[$i] 1:$list[$i+1] 2:$list[$i+2] 3:$list[$i+3]" if ($debug);
# list:
#  0: CumulativeRemoteUserCpu = 0.0
#  1:GlobalJobId = "rcas6006.rcf.bnl.gov#163807.0#1513628765"
#  2:Iwd = "/afs/rhic.bnl.gov/star/users/fisyak/Pico/2016/115/17115061"
#  3:RemoteUserCpu = 0.0
  my $hold = 0;
  my ($dum1,$node) = split('"',$list[$i+1]); print "dum1 = $dum1, dum2 = $dum2 from $list[$i+1]\n" if ($debug);
  $node =~ s/\..*//;
  print "node  $node from $list[$i+1]" if ($debug);
  if ($list[$i+1] =~ /HoldReasonCode/) {
    $hold = 1;
    $i++;
  }
  my ($dum3,$pwd) = split('"',$list[$i+2]);
  $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/work/##;
  $pwd =~ s#/gpfs01/star/pwg/fisyak/##;
  $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/pwg/##;
  $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/##;
  print "pwd: $pwd from $list[$i+2]" if ($debug);
  my ($dum4,$dum5,$cpu) = split(' ',$list[$i+3]);
  print "cpu: $cpu from $list[$i+3]" if ($debug);
  my $key = $node . ":" . $pwd;
  $Hash{$key}++; print "Hash\{$key\} = $Hash{$key}\n" if ($debug);
  if ($cpu > 0.0) {
    $HashR{$key}++; print "HashR\{$key\} = $HashR{$key}\n" if ($debug);
  }
  if ($hold) {
    $HashH{$key}++; print "HashH\{$key\} = $HashH{$key}\n" if ($debug);
  }
}
my $total = 0;
my $run   = 0;
my $hold  = 0;
foreach my $key ( sort keys %Hash ) {
  if ($Hash{$key}) {
    printf("%-60s = %6i runs = %6i/hold = %6i\n",$key,$Hash{$key},$HashR{$key},$HashH{$key});
    $total += $Hash{$key};
    $run   += $HashR{$key};
    $hold  += $HashH{$key};
  }
}
printf("Total                                                        = %6i runs = %6i/hold = %6i\n",$total,$run,$hold);
