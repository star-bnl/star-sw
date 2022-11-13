#! /usr/bin/env perl
#  CheckLog4DbEntry.pl  | sort -u | more
use File::Basename;
my @TableList = qw(TpcSecRowB TpcAccumulatedQ TpcZCorrectionC TpcPadCorrectionMDF TpcLengthCorrectionMDN);
my $TableList = join("|",@TableList);
my @LogFiles = glob "*.log";
foreach my $log (@LogFiles) {
  open(IN, "<$log") or die "Can't open $log";
  my @TableHash - ();
  while (my $line = <IN>) {
    if ($line !~ /$TableList/) {next;}
    if ($line =~ /Unrecognised/) {next;}
    # print "$line\n";
    if ($line !~ /LoadTable/ && $line !~ /Load TFile/ && ! ($line =~ /found table/ && $line =~ /Validity/)) {next;}
    # print "========= $line\n";
    my $table = "";
    foreach my $t ( @TableList ) {
      if ($line =~ /$t/) {$table = $t; last;}
    }
    chomp $line;
#    print "$line\n";
    if ($TableHash{$table}->{Load} && $TableHash{$table}->{Validity}) {next;}
    if ($line =~ /LoadTable/ || $line =~ /Load TFile/) {
      my ($dum,$dum,$Load) = split(" ", $line);
      $TableHash{$table}->{Load} = $Load;# print "Load = $Load\n";
    }
    if ($line =~ /Validity/) {
      my $Val = $line;                           # print "Val = $Val\n";
      $Val =~ s/StdEdxY2Maker:WARN.*Validity://; # print "Val = $Val\n";
      $Val =~ s/ - .*$//;                        # print "Val = $Val\n";
      my ($d,$t) = split("/",$Val);                
      my $Validity = sprintf("%08i.%06i",$d,$t);
      # print "$Val => $d $t => $Validity\n";
      $TableHash{$table}->{Validity} = $Validity; #print "Validity = $Validity\n";
    }
    # print "Found $table  $TableHash{$table}->{Load} $TableHash{$table}->{Validity}\n";
  }
  print "----------------------------------------\n";
  foreach my $table ( @TableList ) {
    if ($TableHash{$table}->{Validity} && $TableHash{$table}->{Validity}) {
      print "$table.$TableHash{$table}->{Validity} $TableHash{$table}->{Load} $log\n";
    } else {
      print "Missed:\t $table.$TableHash{$table}->{Validity} $TableHash{$table}->{Load} $log\n";
    }
  }
  print "----------------------------------------\n";
}
