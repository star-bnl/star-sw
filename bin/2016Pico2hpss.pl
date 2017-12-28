#! /usr/bin/env perl
# cd 076; 2016Pico2hpss.pl | hsi
    use File::Basename;
    use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename($pwd);# print "day = $day\n";
# find . -newer hpss.log -name "*.root" -exec ls -l {} \;
my @lines = glob "*/*.root";# print "lines @lines\n";
print "cd reco/2016Pico\n";
print "mkdir $day\n";
print "cd $day\n";  
foreach my $file (@lines) {
  next if $file !~ /root$/ && $file !~ /gz$/; 
  my $run = File::Basename::dirname($file);
  my $f = File::Basename::basename($file);
#  if ($run !~ /17128002|17128003|17128006|17128007|17128008|17128009|17128010/) {next;}
  print "mkdir $run\n";
  print "cd $run\n";
  print "lcd $run\n";
  print "cput $f\n";
  print "cd ..\n";
  print "lcd ..\n";
#  die;
}
