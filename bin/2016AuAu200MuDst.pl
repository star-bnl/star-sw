#! /usr/bin/env perl
 use File::Basename;
#print "$#ARGV\n";
if ($#ARGV < 0) {
  print "Usage : $0 day\n";
  exit 0;
}
my $debug = 0;
my $day = $ARGV[0]; print "requested day = $day\n" if ($debug);
my $trigger = "";
if ($day <= 130) {
  $trigger = "AuAu_200_production_2016";
} else {
  $trigger = "AuAu200_production2_2016";
}
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_,tpx=1,sanity=1,events>10,runnumber>16140032,trgsetupname=fixedTarget2015' -limit 0`; 
#get_file_list.pl -keys path,filename -cond production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,filename~st_physics,storage=nfs -limit 0
#my  @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,filename~st_physics,tpx=1,pxl=1,ist=1,sanity=1,runnumber>17124000,events>10' -limit 0`;
#my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,tpx=1,pxl=1,ist=1,sanity=1,events>10,runnumber>17000000' -limit 0"; print "$cmd\n" if ($debug);
my $TriggerFile = $trigger . ".txt"; print "Trigger file = $TriggerFile\n" if ($debug);
if (! -r $TriggerFile) {
  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'production=P16ij,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics,tpx=1,pxl=1,ist=1,sanity=1,events>10,runnumber>17000000' -limit 0"; 
  print "$cmd\n" if ($debug);
  my  @list = `$cmd`;
  #Runs rejected due to HFT firmware issues:
  #RunId < 17062047, 17065002 ≤ RunId ≤ 17068053, 17063041 ≤ RunId ≤ 17063043
  #Runs rejected based on the QA:
  my @BadRuns = qw(
		    17063037 17070011 17073049 17073063 17075108 17075110 17076001 17076003 17076010 17097024
		    17097026 17098033 17098035 17098036 17098037 17098038 17098039 17098040 17098043 17098045
		    17098046 17098047 17098048 17098050 17103017 17103059 17103060 17103061 17104001 17104002
		    17104003 17104004 17104005 17104006 17104007 17104008 17104009 17104011 17104012 17104013 
		    17104014 17104015 17104016 17104017 17104018 17112054 17114044 17114045 17114046 17114047 
		    17114049 17114050 17114051 17115001 17115002 17115003 17115004 17115005 17115007 17115008
		    17115009 17115010 17115012 17115013 17115014 17115016 17115018 17115019 17115021 17115022
		    17115025 17115026 17115028 17115030 17115031 17115032 17115033 17115035 17115037 17115038
		    17116006 17116047 17116048 17116049 17116050 17116052 17116053 17116054 17116056 17116057
		    17116058 17116059 17116061 17116062 17116063 17116065 17117003 17117006 17117007 17117008
		    17117010 17117011 17117012 17117013 17117014 17117015 17117019 17117020 17117021 17117023
		    17117028 17117029 17118051 17119001 17119002 17119003 17119004 17119005 17119006 17119007
		    17119009 17119011 17119012 17119013 17119014 17119015 17119016 17119072 17124032 17124039
		    17124040 17124041
		 );
  my $BadRuns = join '|', @BadRuns;
  open(Out, ">$TriggerFile") or die "Can't open $TriggerFile";
  foreach my $line (@list) {
    chomp($line);
    my $file = File::Basename::dirname($line);
    my $events = File::Basename::basename($line);
    my @words = split('/',$file);
    my $year = int $words[7];
    my $dayr = int $words[8]; 
    print "file = $file, year = $year, day = $dayr, events = $events\n" if ($debug);
#    if ($dayr ne $day) {next;}
    my $run =  int $words[9]; 
    if ($run < 17062047 or
	17065002 <= $run && $run <= 17068053 or
	17063041 <= $run && $run <= 17063043) {next;}
    if ($run =~ $BadRuns) {next;}
    #  my $year = int $run/1000000;
    #  my $day = int $run/1000 - 1000 * $year;
    print Out "$file:$events\n";
    print "$file:$events\n" if ($debug);
  }
  close (Out);
}
my $i = 1;   
my $OldRun = 0;
open(In, "$TriggerFile") or die "Can't open $TriggerFile";
while (my $line = <In>) { 
  chomp($line);
  my ($file,$events) = split(":",$line);
  print "$line => $file with $events events\n" if ($debug);
  my @words = split('/',$file);
  my $year = int $words[7];
  my $dayr = int $words[8]; 
  if ($dayr ne $day) {next;}
  my $run =  int $words[9]; 
  print "run = $run year = $year day = $day \n" if ($debug);
  if ($run != $OldRun) {$i = 1; $OldRun = $run;}
  else                 {$i++;}
#  if ($i%20 != 1) {next;}
  my $basename = $file;
  $basename =~ s#home/starreco/reco/$trigger/ReversedFullField/P16ij/##;
#  print "$file = > $basename\n";
  my $found = 0;
  my $dir;
  my @dirs = qw( /gpfs02/eic/ayk/STAR/reco/MuDst /gpfs01/star/pwg/fisyak/MuDst );
  foreach $dir (@dirs) {
    my $fullpath = $dir . $basename;
    print "$i : $fullpath ===========" if ($debug);
    if (-f $fullpath) {
      $found = 1; 
      print "found\n" if ($debug);
      last;
    }
    print "not found\n" if ($debug);
  }
  if (! $found) {
    my $piconame = $basename;
    $piconame =~ s/MuDst/picoDst/;
    my $pico = "/gpfs01/star/pwg_tasks/hf03/Pico" . $piconame; print "$pico\n" if ($debug);
    if (-r $pico) {
      print "$pico is Done\n" if ($debug);
      next;
    }
    print "$file $dirs[0]$basename\n";
  }
}
close (In);


# awk -F\: 'BEGIN{n= 0}{n += $122}END{printf("%7.3fB\n", n/1.e9)}' AuAu_200_production_2016.txt
