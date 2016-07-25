#! /usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
use Env;

my $CLUSTER = hostname(); print "Cluster $CLUSTER\n";
# star_cas_big
my %ARG = (queue => 'star_cas_prod',
	   limit => '99999',
	   files => '*.csh'
	  );
my @Files = ();
while (@ARGV) {
  my $arg = shift @ARGV;
#  print ("arg = $arg\n");
  if ($arg =~ /=/) {
    $_ = $arg;
    my($key, $val) = /([^=]*)=(.*)/;
#    print "key=$key \tval = $val\n";
    $val = "star_cas_big" if $val =~ /^big/;
    $val = "star_cas_short" if $val =~ /^short/;
    $val = "star_cas_prod" if $val =~ /^prod/;
    $ARG{$key} = $val;
  } else {
    push @Files, $arg;
  }
}
#die;
print("queue = $ARG{queue}, limit = $ARG{limit}, files = $ARG{files}\n");
#print("Files = $#Files\n");
my $job = 0;
if ($#Files < 0) {
  @Files = glob $ARG{files};
  print("Files = $#Files\n");
}
print("Files = $#Files\n");
foreach my $file (@Files) {
  my $flag = system("chmod +x $file");
  my $jobn = File::Basename::basename($file,".csh");
  my $log  = $jobn . ".log";
  my $logB  = $jobn . "B.log";
  my $rootg = $jobn;
  $rootg =~ s/st_physics_//;# print "pattern $rootg\n";
  $rootg = "*" . $rootg . "*.root";
  my @root = glob $rootg ;# print "$rootg => @root\n";
  my $rootn = $#root;
#  if ($rootn != -1) {print "$rootn => @root\n";}
  if (-f $log or -f $logB or $rootn != -1) {
    print "$log and @root files are already exists. Skip.\n";
  } else {
    #    bsub -N -u fisyak@bnl.gov -o $log -q $QUEUE  $file 
    my $JobF = $jobn . ".job";
    my $pwd = cwd();
    open (Out,">$JobF") or die "Can't open $JobF\n";
    print Out '
# Example of a new Condor submit file. Note that this
# job specifically requests a fast execute node:
#
# CPU_Speed == 1 -> slow 
# CPU_Speed == 2 -> medium 
# CPU_Speed == 3 -> fast
# 
Universe        = vanilla 

Notification    = Complete
Executable      = ' . $pwd . '/' . $file . '
#Arguments       = ' . $HOME . '
Requirements    = CPU_Speed >= 2
# Image_Size      = 128 Meg
Priority        = +20
GetEnv          = True

Initialdir      = ' . $pwd  . '
Input           = /dev/null
Outpit          = ' . $pwd . '/' . $jobn . '.out
Error           = ' . $pwd . '/' . $jobn . '.err
Log             = ' . $pwd . '/' . $jobn . '.log
Owner           = ' . $USER . '
Notify_user     = '  . $USER . '@bnl.gov

+Experiment     = "star"
+Job_Type       = "cas"

Queue
';
#    my $cmd = "bsub -J $jobn -o $log -q $ARG{queue}  $file"; 
    my $cmd = "condor_submit " . $JobF;
    print "$cmd\n";
    $flag = system($cmd);
#    sleep 5;
  $job++;
  }
#  print ("job = $job, limit =  $ARG{limit} \n");
  if ($job >= $ARG{limit}) {
    print "limit has been reached\n";
    last;
  }
}
