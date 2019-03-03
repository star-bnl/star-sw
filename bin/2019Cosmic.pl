#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd(); #print "pwd = $pwd\n";
if ($pwd =~ /FF/)    {$FIELD = "FF";}
elsif ($pwd =~ /RF/) {$FIELD = "RF";}
else {die "Cannot define field";}
#print "FIELD = $FIELD\n";
if (! $FIELD) {die "Field is not defined";}
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>16161045,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=FullField,filetype=online_daq,trgsetupname=CosmicLocalClock,tpx=1,pxl=1,ist=1,gmt=1,sanity=1,events>10000' -limit 50`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=ReversedFullField,filetype=online_daq,trgsetupname=CosmicLocalClock,tpx=1,pxl=1,ist=1,gmt=1,sanity=1,events>10000' -limit 50`;
# if ($#ARGV < 0) {
#   print "Usage $0 ZF, FF or RF\n";
#   exit 0;
# }
# 20018043
# my $field = "";
# my $Field = $ARGV[0];
# print "$Field\n";
# if    ($Field eq "FF") {$field = "FullField";}
# elsif ($Field eq "RF") {$field = "ReversedFullField";}
# elsif ($Field eq "ZF") {$field = "FieldOff"; }
# print "$Field => $field\n";
# if ($field eq "") {die "$Field has not defined";}
# my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=" . $field . ",filetype=online_daq,filename~st,trgsetupname=CosmicLocalClock,tpx=1,gmt=1,sanity=1,runnumber>19000000' -limit 0"; 
# print "$cmd\n";
# my @list = `$cmd`;
# #my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=ReversedFullField,filetype=online_daq,filename~st_gmt,trgsetupname=CosmicLocalClock,tpx=1,gmt=1,sanity=1,events>10000' -limit 50`;
#my @list = glob "/net/l401/data/scratch1/daq/2019/*/*/st_cosmic_*.daq";
my @list = glob "/hlt/cephfs/daq/2019/*/*/st*cosmic_*.daq /hlt/cephfs/daq/2019/*/*/st*gmt*.daq";
# FF = 20028045 - 20035016
# RF = 20035041
my $runMin = -1;
my $runmax = -1;
if    ($FIELD eq "FF") {$runMin = 20018043; $runMax = 20035016;}
#elsif ($FIELD eq "RF") {$runMin = 20035048; $runMax = 20999999;}
elsif ($FIELD eq "RF") {$runMin = 20048035; $runMax = 20999999;}
else  {die "Field has not been defined";}
foreach my $file (@list) {
  chomp($file);
  my $bf = File::Basename::basename($file,".daq");
  my $f = File::Basename::dirname($file);
  my $run = File::Basename::basename($f);
#  print "runMin = $runMin , run = $run , runMax = $runMax \n";
  if ($run < $runMin || $run > $runMax) {next;}
  my $root = $bf . ".MuDst.root"; #print "root = $root\n";
  my $roott = "./MuDst/" . $root; #print "root = $root\n";
  if (-r $root || -r $roott) {next;}
  print "string:$file\n";
#  last;
}

