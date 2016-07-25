#! /usr/bin/env perl
    use File::Basename;
#my $list = $ARGV[0];
my %Files = ();
my $histolist = "Histograms.list";
my $answer = `hsi "cd Histograms; out $histolist; ls -1"`;
my $flag = system($answer);
if (! -r $histolist) {
  #  die "$histolist does not exist";
  my $answer = `hsi "cd Histograms; out $histolist; ls -1"`;
}
open (In, $histolist) or die "Can't open $list";
while (my $line = <In>) {
#  my @words = split ' ',$line;
#  my $file = $words[8]; print "$file\n";
  my $file = File::Basename::basename($line); 
  chomp($file);print "$file\n";
  next if $file !~ /\.root$/;
  $Files{$file} = "YES";
}
close(In);
my $outfile = "put.csh";
open (Out, ">$outfile") or die "Can't open $outfile";
#print "rftp \<\<EOF\n cd Histograms\n";
#print Out "rftp \<\<EOF\n cd Histograms\n";
my @file_list = glob "*.root";
foreach my $file (@file_list) {
  next if $Files{$file};
  print "put $file : Histograms/$file\n";
  print Out " hsi put $file : Histograms/$file\n";
}
#print "bye\nEOF\n";
#print Out "bye\nEOF\n";
close (Out);
