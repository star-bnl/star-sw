#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
#if ($#ARGV < 0) {
#  print "Usage: $0 files='*.root' FilesPerJob='100' Out='hadd_files'\n";
#  exit 0;
#} 
my %ARG = (files => '*/*.root',
           FilesPerJob => '10',
	   Out => 'hadd',
	   version => '.DEV2'
	  );
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}

while (my ($key,$value) = each %ARG) {
  print  "$key=$value\n";
}
my $XML = "hadd.xml";
open (XML,">$XML") or die "Can't open $XML";
#
print XML '<?xml version="1.0" encoding="utf-8" ?> 
<job name="recoJob" maxFilesPerProcess="1" filesPerHour="2" simulateSubmission="false" fileListSyntax="paths">
     <command>csh -x $INPUTFILE0 </command>
     <stdout URL="file:' . $DIR . '/shed$JOBID.log" />
';
my $glob = $ARG{files}; print "glob = $glob\n"; 
my $FilesPerJob = $ARG{FilesPerJob}; print "FilesPerJob = $FilesPerJob\n";
my $Out = $ARG{Out}; print "Out = $Out\n";
my @Files = glob "$glob"; 
print "no of files : $#Files\n"; 
next if $#Files < 0;
my %runs = ();
foreach my $file (@Files) {
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, 
      $mtim, $ctime, $blksize,$blocks) = stat($file);
  next if $size < 500000; # 0.5 MB limit
  my $f = File::Basename::basename($file); #print "$file => $f\n";
  $f =~ s/adc_//;
  $f =~ s/st_minbias_//;
  $f =~ s/st_physics_//;
  my ($run) = split '_', $f;
#  print "$file => $f => $run\n";
  if ($runs{$run}) {
    $runs{$run} .= " " . $file;
  } else {
    $runs{$run} = $file;
  }
#  print "$run => $runs{$run}\n";
}
foreach my $key( sort keys %runs) {
  my $name = $key;
  my $SCRIPT = $name . ".csh";
  next if -r $SCRIPT;
  my $list = $runs{$key};
#  my $cmd = "hadd " . $key . ".root $list";
  my $cmd = "root.exe -q -b " . $list;
  $cmd .= " 'Hadd.C(\"" . $key . ".root"  . "\")'";;
  print "job:$key: => $cmd \n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print "Create $SCRIPT\n";
  print OUT "#!/bin/tcsh -v\n";
  print OUT "cd $DIR\n";
  print OUT "starver " . $ARG{version} . "\n";
  print OUT "$cmd\n";
  print OUT "if (\$? == 0) rm $list;\n";
  close (OUT);
  print XML "<input URL=\"file:" . $DIR . "/" .  $SCRIPT ."\" />\n";
}
print XML '
</job>
';
close (XML);

