#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $macro = "RunTbyT";
my $debug = 0;
my @FilesNew = glob "new/*.event.root"; print "FilesNew @FilesNew\n" if $debug;
my @FilesOld = glob "old/*.event.root"; print "FilesOld @FilesOld\n" if $debug;

my $newfiles = "";
my $oldfiles = "";
foreach my $f (@FilesNew) {
  my $bf = File::Basename::basename($f);
  $bf =~ s/\.event\.root//;
#  print "$bf\n";
  $bf =~ s/_\d\d\d\d_\d\d\d\d$//;
  if (! $newfiles) {$newfiles = $bf;}
  elsif ($newfiles !~ $bf) {
    $newfiles .= "|" .$bf;
#    print "$f => $bf => $newfiles\n";
  }
}
foreach my $f (@FilesOld) {
  my $bf = File::Basename::basename($f);
  $bf =~ s/\.event\.root//;
#  print "$bf\n";
  $bf =~ s/_\d\d\d\d_\d\d\d\d$//;
  if (! $oldfiles) {$oldfiles = $bf;}
  elsif ($oldfiles !~ $bf) {
    $oldfiles .= "|" .$bf;
#    print "$f => $bf => $oldfiles\n";
  }
}
$newfiles =~ s/\|/ /g;
$oldfiles =~ s/\|/ /g;
@FilesNew = split(' ',$newfiles); print "FilesNew @FilesNew\n" if $debug;
@FilesOld = split(' ',$oldfiles); print "FilesOld @FilesOld\n" if $debug;
my %Pair = (
	    'new'              => 'old'
	   );
my %Files = {};
foreach my $newf (@FilesNew) {
  my $nff = File::Basename::basename($newf);
#  $nff =~ s/\.event\.root//;
  my $nf = $nff;
#  $nf =~ s/_1_500//;
#  $nf =~ s/_501_1000//;
#  print "nf = $nf\n";
  foreach my $oldf (@FilesOld) {
    my $off = File::Basename::basename($oldf);
#    $off =~ s/\.event\.root//;
    my $of = $off;
#    $of =~ s/_1_500//;
#    $of =~ s/_501_1000//;
#    print "of = $of\n";
    if ($nff eq $off) {$nf = $nff; $of = $off;}
    next if $nf !~ /$of/;
    print "================\n$oldf => $nf\n$newf => $of\n";
    my $key = $nf;
    my $first = $oldf;
    my $second = $newf;
    print "$key => $first \t $second\n";
    my $SCRIPT = $key;
    my $LOG = $SCRIPT . ".log";
    my $root = $SCRIPT;
    my $RootFile = "trackMateFile" . $root . ".root";
    next if -r $RootFile;
    $SCRIPT .= ".csh";
    my $file1 =  $first;
    my $file2 =  $second;
    print "Create $SCRIPT\n";
    open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
    print OUT "#! /usr/local/bin/tcsh -f\n";
#    print OUT "source /afs/.rhic.bnl.gov/star/group/.starver .DEV2;\n";
    my $cmd = "";
    $cmd .= "test ! -r " . $RootFile  . " && root -l -q -b  '" . $macro;
    $cmd .= ".C(1000000,\"old/" . $file1 . "*.event.root\",\"new/" . $file2 . "*.event.root\",\"" . $RootFile  . "\")\' >& $LOG";
    print OUT "$cmd\n";
    close (OUT);
  }
}

