#!/usr/bin/perl
#
use Getopt::Std;
use Cwd;
use File::Basename;
use strict;
use vars qw(%opt);
#_______________________
my $usage =<<EOF;
Usage: $0 [options] <hijing|embed> <inputdir> <outputdir> [logDir]
options :
     -t           test
     -o           overwrite
     -m <regexp>  match this regexp
     -q <queue>
     -r <io usage>
     -d <disk vault for io usage> (e.g. 05)
     -n <max files>
     -l <library version>
     -a <string added to logfile>
     -z <adjust output name when skipping>
EOF

getopts('otl:n:m:q:s:r:u:d:a:z:',\%opt);

my $type  = shift; # 'hijing' or 'embed'
my $inDir  = shift; # input dir of geant files
my $outDir = shift; # output dir of minimc root files
my $logDir = shift || "LOG";

if(!defined $outDir and defined $inDir){
  ($outDir = $inDir) =~ s/\.dst\/?$/\.mdst/;
  die "Cannot find $outDir :$!\n" if ($outDir eq $inDir);
}
my $star = basename $ENV{'STAR'};
my $library = $opt{l} || $star;
my $nEvent = 99999;
my $commonHits = 3;
my $queue = $opt{q} || ($ENV{HOST} =~ /pdsf/ ? "short" : "star_cas");
my $io = $opt{r} || 40;
my $max  = $opt{n} || 10000;
my $count = 0;
#my $scriptDir = "StRoot/StMiniMcMaker";
my $scriptDir = "minimcscripts";
my $workDir = "/auto/pdsfdv34/starspec/calderon/minimctrees";
my $macroName;
my $ext = ".geant.root";
my $user = $opt{u} ? "-u $opt{u}" : '-u bum@rcf.rhic.bnl.gov';
my $disk = $opt{d} || "45";
my $add = ($opt{a} =~ /^\./) ? $opt{a} : (defined $opt{a} ? "$opt{a}." : "");

(defined $inDir and defined $outDir) or die "$usage\n";

-e $inDir or die "$inDir : $!\n";
-e $outDir or die "$outDir : $!\n";
-e $logDir or die "$logDir : $!\n";
-d $scriptDir or die "$scriptDir : $!\n";

print "Using...\n",
  "in dir : $inDir\n",
  "out dir: $outDir\n",
  "log dir: $logDir\n",
  "libary ver : $library\n";


if($type eq 'hijing'){
  $macroName = 'StRoot/StMiniMcMaker/StMiniHijing.C';
}
elsif($type eq 'embed'){
  $macroName = 'StRoot/StMiniMcMaker/StMiniEmbed.C';
}
else{
  die "$usage\n";
}

opendir(DH,$inDir) or die "$inDir : $!\n";

my @all = glob("$inDir/*");
foreach my $dir1 (@all) {
  #print "$dir1\n";
  opendir(DG,$dir1) or die "$dir1 : $!\n";
  while (my $file = readdir DG) {
    #print "$file\n";
    next if $file =~ /^\./;
    next unless $file =~ /geant.root/;
    
    if($opt{m}){
      next unless $file =~ /$opt{m}/;
    }
    (my $found = $file ) =~ s/geant.root/minimc.root/;
    if($opt{z}){
      $found =~ /st_physics_(\d+)_(\S+)/;
      $found = "st_physics_$1.$opt{z}_$2";
    }
    unless($opt{o}){
      next if -e "$outDir/$found";
    }
    last if ++$count>$max;
    
    (my $label = $file) =~ s/$ext$//;
    
    my $mainFile = "$dir1/$label.geant.root";
    #print "$mainFile\n";
    # find the corresponding dst.root file
    (my $dst = $mainFile) =~ s/\.geant\.root/\.dst\.root/;
    unless(-e $dst){
      print "\tCannot find $dst\n"; next;
    }
    
    (my $base = $dir1) =~ s/\/?$//;
    $base = basename $base; $base =~ s/\.dst$//;
    my $scriptName = "$scriptDir/$base.$add$label.csh";
    my $logName = "$logDir/$base.$add$label.log";
    my $jobName = "$base.$add$label";
    
    my $cwd = cwd();
    
    unless($opt{t}){
      local *FH;
      open(FH,">$scriptName") or die "$scriptName : $!\n";
      
      my $text=<<FINIS;
#!/bin/csh

source \$GROUP_DIR/.starver $library
cd $workDir
set start = `date`
root4star -b <<EOF >&! $logName
char* mainFile = "$mainFile"
char* outdir   = "$outDir"
.x $macroName(99999,mainFile,outdir,3)
.q
EOF

FINIS
      print FH $text;
      close FH;
      chmod 0754, $scriptName;
    }

    my $extra;
    if($ENV{HOST} =~ /pdsf/){
      $extra = qq{ -R "select[defined(dv${disk}io)] rusage[dv${disk}io=$io]"};
    }
    
    my $command = "bsub -q $queue -J $jobName $extra $scriptName";
    print $command, "\n";
    system($command) unless $opt{t};
  }
}
