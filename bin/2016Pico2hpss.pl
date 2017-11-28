#! /usr/bin/env perl
# cd 076; 2016Pico2hpss.pl | hsi
    use File::Basename;
    use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename($pwd);# print "day = $day\n";
my @lines = glob "*/*.root";# print "lines @lines\n";
print "cd reco/2016Pico\n";
print "mkdir $day\n";
print "cd $day\n";  
foreach my $file (@lines) {
  next if $file !~ /root$/ && $file !~ /gz$/; 
  my $run = File::Basename::dirname($file);
  my $f = File::Basename::basename($file);
#  if ($run =~ /17130001|17130002|17130003/) {next;}
#  if ($run =~ /17130004/) {
#    my @qwerty = qw(
#st_physics_17130004_raw_1000003.picoDst.root   st_physics_17130004_raw_2000058.picoDst.root   
#st_physics_17130004_raw_1000013.picoDst.root   st_physics_17130004_raw_2000059.picoDst.root   
#st_physics_17130004_raw_1000016.picoDst.root   st_physics_17130004_raw_2000061.picoDst.root   
#st_physics_17130004_raw_1000018.picoDst.root   st_physics_17130004_raw_2500002.picoDst.root   
#st_physics_17130004_raw_1000021.picoDst.root   st_physics_17130004_raw_2500012.picoDst.root   
#st_physics_17130004_raw_1000023.picoDst.root   st_physics_17130004_raw_2500013.picoDst.root   
#st_physics_17130004_raw_1000026.picoDst.root   st_physics_17130004_raw_2500017.picoDst.root   
#st_physics_17130004_raw_1000027.picoDst.root   st_physics_17130004_raw_2500019.picoDst.root   
#st_physics_17130004_raw_1000029.picoDst.root   st_physics_17130004_raw_2500023.picoDst.root   
#st_physics_17130004_raw_1000031.picoDst.root   st_physics_17130004_raw_2500024.picoDst.root   
#st_physics_17130004_raw_1000033.picoDst.root   st_physics_17130004_raw_2500030.picoDst.root   
#st_physics_17130004_raw_1000037.picoDst.root   st_physics_17130004_raw_2500032.picoDst.root   
#st_physics_17130004_raw_1000040.picoDst.root   st_physics_17130004_raw_2500034.picoDst.root   
#st_physics_17130004_raw_1000041.picoDst.root   st_physics_17130004_raw_2500035.picoDst.root   
#st_physics_17130004_raw_1000044.picoDst.root   st_physics_17130004_raw_2500040.picoDst.root   
#st_physics_17130004_raw_1000045.picoDst.root   st_physics_17130004_raw_2500041.picoDst.root   
#st_physics_17130004_raw_1000047.picoDst.root   st_physics_17130004_raw_2500044.picoDst.root   
#st_physics_17130004_raw_1000048.picoDst.root   st_physics_17130004_raw_2500046.picoDst.root   
#st_physics_17130004_raw_1000052.picoDst.root   st_physics_17130004_raw_2500048.picoDst.root   
#st_physics_17130004_raw_1000053.picoDst.root   st_physics_17130004_raw_2500049.picoDst.root   
#st_physics_17130004_raw_1000057.picoDst.root   st_physics_17130004_raw_2500050.picoDst.root   
#st_physics_17130004_raw_1000058.picoDst.root   st_physics_17130004_raw_2500052.picoDst.root   
#st_physics_17130004_raw_1500002.picoDst.root   st_physics_17130004_raw_2500058.picoDst.root   
#st_physics_17130004_raw_1500013.picoDst.root   st_physics_17130004_raw_2500059.picoDst.root   
#st_physics_17130004_raw_1500016.picoDst.root   st_physics_17130004_raw_2500062.picoDst.root   
#st_physics_17130004_raw_1500018.picoDst.root   st_physics_17130004_raw_2500063.picoDst.root   
#st_physics_17130004_raw_1500019.picoDst.root   st_physics_17130004_raw_2500064.picoDst.root   
#st_physics_17130004_raw_1500022.picoDst.root   st_physics_17130004_raw_3000003.picoDst.root   
#st_physics_17130004_raw_1500023.picoDst.root   st_physics_17130004_raw_3000008.picoDst.root   
#st_physics_17130004_raw_1500026.picoDst.root   st_physics_17130004_raw_3000014.picoDst.root   
#st_physics_17130004_raw_1500029.picoDst.root   st_physics_17130004_raw_3000016.picoDst.root   
#st_physics_17130004_raw_1500030.picoDst.root   st_physics_17130004_raw_3000023.picoDst.root   
#st_physics_17130004_raw_1500032.picoDst.root   st_physics_17130004_raw_3000024.picoDst.root   
#st_physics_17130004_raw_1500033.picoDst.root   st_physics_17130004_raw_3000025.picoDst.root   
#st_physics_17130004_raw_1500039.picoDst.root   st_physics_17130004_raw_3000028.picoDst.root   
#st_physics_17130004_raw_1500040.picoDst.root   st_physics_17130004_raw_3000032.picoDst.root   
#st_physics_17130004_raw_1500042.picoDst.root   st_physics_17130004_raw_3000034.picoDst.root   
#st_physics_17130004_raw_1500045.picoDst.root   st_physics_17130004_raw_3000035.picoDst.root   
#st_physics_17130004_raw_1500046.picoDst.root   st_physics_17130004_raw_3000036.picoDst.root   
#st_physics_17130004_raw_1500050.picoDst.root   st_physics_17130004_raw_3000040.picoDst.root   
#st_physics_17130004_raw_1500052.picoDst.root   st_physics_17130004_raw_3000042.picoDst.root   
#st_physics_17130004_raw_1500053.picoDst.root   st_physics_17130004_raw_3000045.picoDst.root   
#st_physics_17130004_raw_1500057.picoDst.root   st_physics_17130004_raw_3000047.picoDst.root   
#st_physics_17130004_raw_1500058.picoDst.root   st_physics_17130004_raw_3000048.picoDst.root   
#st_physics_17130004_raw_1500059.picoDst.root   st_physics_17130004_raw_3000049.picoDst.root   
#st_physics_17130004_raw_2000002.picoDst.root   st_physics_17130004_raw_3000054.picoDst.root   
#st_physics_17130004_raw_2000010.picoDst.root   st_physics_17130004_raw_3000055.picoDst.root   
#st_physics_17130004_raw_2000013.picoDst.root   st_physics_17130004_raw_3000056.picoDst.root   
#st_physics_17130004_raw_2000018.picoDst.root   st_physics_17130004_raw_3000059.picoDst.root   
#st_physics_17130004_raw_2000019.picoDst.root   st_physics_17130004_raw_3000063.picoDst.root   
#st_physics_17130004_raw_2000021.picoDst.root   st_physics_17130004_raw_3000064.picoDst.root   
#st_physics_17130004_raw_2000026.picoDst.root   st_physics_17130004_raw_3000065.picoDst.root   
#st_physics_17130004_raw_2000027.picoDst.root   st_physics_17130004_raw_3000068.picoDst.root   
#st_physics_17130004_raw_2000029.picoDst.root   st_physics_17130004_raw_3000070.picoDst.root   
#st_physics_17130004_raw_2000030.picoDst.root   st_physics_17130004_raw_3000071.picoDst.root   
#st_physics_17130004_raw_2000031.picoDst.root   st_physics_17130004_raw_3000076.picoDst.root   
#st_physics_17130004_raw_2000036.picoDst.root   st_physics_17130004_raw_3000080.picoDst.root   
#st_physics_17130004_raw_2000038.picoDst.root   st_physics_17130004_raw_3000081.picoDst.root   
#st_physics_17130004_raw_2000042.picoDst.root   st_physics_17130004_raw_3000082.picoDst.root   
#st_physics_17130004_raw_2000044.picoDst.root   st_physics_17130004_raw_3000084.picoDst.root   
#st_physics_17130004_raw_2000045.picoDst.root   st_physics_17130004_raw_3000088.picoDst.root   
#st_physics_17130004_raw_2000046.picoDst.root   st_physics_17130004_raw_3000089.picoDst.root   
#st_physics_17130004_raw_2000049.picoDst.root   st_physics_17130004_raw_3000090.picoDst.root   
#st_physics_17130004_raw_2000051.picoDst.root   st_physics_17130004_raw_3000091.picoDst.root   
#st_physics_17130004_raw_2000053.picoDst.root   st_physics_17130004_raw_3000095.picoDst.root   
#		  );
#    my $qwerty = join '|', @qwerty;
#    if ($f =~ $qwerty) {next;}
#  }
  print "mkdir $run\n";
  print "cd $run\n";
  print "lcd $run\n";
  print "cput $f\n";
  print "cd ..\n";
  print "lcd ..\n";
}
