#! /usr/bin/env perl
my @cshells = glob "sched*.csh";
my @condor = `condor_q -wide -submitter fisyak | grep -w H`;
my $N = 0;
my $list = "";
my $debug = 0;
foreach my $condor (@condor) {
  print "condor = $condor\n" if ($debug);
  my @words = split(' ',$condor); 
  my $r = $words[8]; print "r = $r\n" if ($debug);
  foreach my $csh (@cshells) {
    print "csh = $csh\n" if ($debug);
    my ($dum,$id) = split("_",$csh);
    $id =~ s/\.csh//;  print "id = $id\n" if ($debug);
    print "csh = $csh and r = $r\n" if ($debug);
    if ($r ne $csh) {next;}
    my ($dum,$rid) = split("_",$r); print "rid = $rid\n" if ($debug);
    $rid =~ s/\.csh//;  print "rid = $rid\n" if ($debug);
    if ($list eq "") {$list .= $id;}
    else             {$list .= "," . $id};
  }
}
if ($list ne "") {
  print "lsf -r $list *.xml\n";
}
