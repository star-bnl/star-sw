#! /usr/bin/env perl
# cd 076; 2013W2hpss.pl | hsi
    use File::Basename;
    use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename($pwd);# print "day = $day\n";
my @lines = glob "*/*.root";# print "lines @lines\n";
print "cd reco/2014/50M/SL15StiCAKFV/PicoDST\n";
print "mkdir $day\n";
print "cd $day\n";  
foreach my $file (@lines) {
  my $d = File::Basename::dirname($file);
  print "mkdir $d\n";
  print "cd $d\n";
  print "lcd $d\n";
  my $f = File::Basename::basename($file);
  print "cput $f\n";
  print "cd ..\n";
  print "lcd ..\n";
}
