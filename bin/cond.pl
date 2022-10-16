#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $debug = 0;
#my @list = ` condor_q -l -s fisyak | egrep '(GlobalJobId|NumCkpts_RAW|Iwd|ClusterId|RemoteUserCpu =)'`;
#my @list = ` condor_q -global -l -s fisyak | egrep '(GlobalJobId|Iwd|RemoteUserCpu =|HoldReasonCode)'`;
#my @list = ` condor_q -global -l -s fisyak | egrep '(Arguments|ClusterId|GlobalJobId|Iwd|RemoteUserCpu =|HoldReasonCode|WhenToTransferOutput)'`;  
my @list = ` condor_q -global -l fisyak | egrep '(Arguments|ClusterId|GlobalJobId|Iwd|RemoteUserCpu =|HoldReasonCode|WhenToTransferOutput)'`;  
#print "@list\n";   
my $line = ""; 
my %Hash = ();
my %HashR = (); # RUN
my %HashH = (); # HOLD
my $GlobalJobId = "";
my $Iwd = "";
my $RemoteUserCpu = 0;
my $hold = 0;
my $cpu = 0;
my $pwd = 0;
my $dum1 = 0;
my $dum2 = 0;
my $dum3 = 0;
my $dum4 = 0;
my $dum5 = 0;
foreach my $line (@list) {
  print "line:\n $line" if ($debug); 
# list:
# Arguments = "" 
# AutoClusterId = 4986 
# ClusterId = 111431 
# CumulativeRemoteUserCpu = 20766.0 
# GlobalJobId = "rcas6005.rcf.bnl.gov#111431.0#1513742809" 
# HoldReasonCode = 34 
# Iwd = "/afs/rhic.bnl.gov/star/users/fisyak/work/reco/2014/TFG17q" 
# RemoteUserCpu = 20766.0 
# WhenToTransferOutput = "ON_EXIT" 
  if ($line =~ /Arguments/) {
    $hold = 0;
    $cpu = 0;
    $pwd = "";
    next;
  } elsif ($line =~ /ClusterId/) {next;
  } elsif ($line =~ /HoldReasonCode/) {
    $hold = 1; 
    print "hold = $hold from $line" if ($debug);
    next;
  } elsif ($line =~ /GlobalJobId/) {
    ($dum1,$node) = split('"',$line); print "dum1 = $dum1, node = $node from $line" if ($debug);
    $node =~ s/\..*//;
    print "node  $node from $line" if ($debug);
  } elsif ($line =~ /Iwd/) {
    ($dum3,$pwd) = split('"',$line);
    $pwd =~ s#/gpfs02/eic/ayk/STAR/reco/##;
    $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/work/##;
    $pwd =~ s#/gpfs01/star/pwg/fisyak/##;
    $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/pwg/##;
    $pwd =~ s#/afs/rhic.bnl.gov/star/users/fisyak/##;
    print "pwd: $pwd from $line" if ($debug);
  } elsif ($line =~ /RemoteUserCpu/) {
    ($dum4,$dum5,$cpu) = split(' ',$line);
    print "cpu: $cpu from $line" if ($debug);
    next;
  } elsif ($line =~ /WhenToTransferOutput/) {
    if (! $node or ! $pwd) {next;}
    my $key = $node . ":" . $pwd;
    $Hash{$key}++; print "Hash\{$key\} = $Hash{$key}\n" if ($debug);
    if ($cpu > 0.0) {
      $HashR{$key}++; print "HashR\{$key\} = $HashR{$key}\n" if ($debug);
    }
    if ($hold) {
      $HashH{$key}++; print "HashH\{$key\} = $HashH{$key}\n" if ($debug);
    }
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
