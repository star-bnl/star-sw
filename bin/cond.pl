#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @list = ` condor_q -l -s fisyak | egrep '(GlobalJobId|NumCkpts_RAW|Iwd|ClusterId|RemoteUserCpu =)'`;
my @list = ` condor_q -l -s fisyak | egrep '(GlobalJobId|Iwd|RemoteUserCpu =|HoldReasonCode)'`;
#my @list = ` condor_q -l -s fisyak | egrep '(GlobalJobId|Iwd|CurrentHosts =|HoldReasonCode)'`;
#print "@list\n";
my $line = "";
my %Hash = ();
my %HashR = (); # RUN
my %HashH = (); # HOLD
my $GlobalJobId = "";
my $Iwd = "";
my $RemoteUserCpu = 0;
for (my $i = 0; $i < $#list; $i += 3) {
  my ($dum1,$dum2) = split('"',$list[$i]);
  my ($node) = split('\.',$dum2);
  #  print "$node from $list[$i]";
  my $hold = 0;
  if ($list[$i+1] =~ /HoldReasonCode/) {
    $hold = 1;
    $i++;
  }
  my ($dum3,$pwd) = split('"',$list[$i+1]);
  $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/work/##;
  $pwd =~ s#/gpfs01/star/pwg/fisyak/##;
  $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/pwg/##;
  $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/##;
  #  print "$pwd from $list[$i+1]";
  my ($dum4,$dum5,$cpu) = split(' ',$list[$i+2]);
  #  print "$cpu from $list[$i+2]";
  $Hash{$node . ":" . $pwd}++;
  if ($cpu > 0.0) {
    $HashR{$node . ":" . $pwd}++;
  }
  if ($hold) {
    $HashH{$node . ":" . $pwd}++;
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
