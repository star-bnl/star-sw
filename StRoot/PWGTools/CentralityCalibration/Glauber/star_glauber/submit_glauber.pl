#! /usr/bin/perl
#
#----------------------------------------------------------------------------------------------------
# Glauber job submission script
#----------------------------------------------------------------------------------------------------

use Getopt::Long ;

my $directory   = "output" ; # Output directory (default is outputs)
my $energy      = 27      ; # sqrt(sNN), default is 200 GeV
my $help        = 0         ; # Help messages flag
my $nevents     = 1000      ; # Number of events (default is 1000)
my $repulsion   = 0.0       ; # repulsion distance (default is 0fm, i.e. no repulsion)
my $run         = 0         ; # Submission flag (default is OFF, i.e. not submit jobs, just print)
my $overwrite   = 0         ; # Overwrite flag (default is false)
my $system      = "AuAu"    ; # System (default is Au+Au)
my $sleep       = 0         ; # Job waiting time (default is 0 sec)
my $type        = "default" ; # Type (default is "default")
my $deformation = "kTRUE"  ; # Deformation flag (default is "kTRUE")
my $verbose     = 0         ; # Debug flag

my $macro      = "doFastGlauberMcMaker.csh" ; # macro in current directory

GetOptions (
    'deformation=s' => \$deformation,
    'directory=s' => \$directory,
    'energy=s' => \$energy,
    'help' => \$help,
    'nevents=s' => \$nevents,
    'repulsion=s' => \$repulsion,
    'run' => \$run,
    'system=s' => \$system,
    'sleep=i' => \$sleep,
    'type=s' => \$type,
    'verbose' => \$verbose
);

my $usage = q(

    Usage: submit_glauber.pl [options]   [Begin run]  [End run]

      Begin/End run should be any integer values, and End run should be larger than Begin run.


    Current available options (in any order):

    -def (or --deformation) [kTRUE or kFALSE] Set deformation flag

    -dir (or --directory) [output dir]  Set output directory (default is outputs)
    -e (or --energy) [energy]           Set energy (default is 200 GeV)
    -h or --help                        Show this messages and exit
    -n (or --nevents) [# of events]     Set number of events (default is 1000)
    -re (or --repulsion) [distance]     Set repulsion distance (default is 0fm, i.e. no repulsion)
    -run                                Submit jobs (default is OFF, i.e. not submit, just print)

    -sys (or --system) [system]         Set system (default is AuAu)
    Current available systems are:
        AuAu
        SmSm

    -sleep [second]                   Wait [second] sec (default is 0 sec, i.e. no wait)

    -t (or --type) [type]             Set type of Glauber simulation (default is "default")
    Current available types are:
        default          Default MC Glauber simulation
        large            Larger radius (+2%), smaller skin depth (-10%)
        small            Smaller radius (-2%), larger skin depth (+10%)
        largeXsec        Larger cross section (+1mb)
        smallXsec        Smaller cross section (-1mb)
        gauss            Gaussian overlap collision profile

    -v or --verbose                   Print debugging messages

);

#----------------------------------------------------------------------------------------------------
# Print help, and exit
#----------------------------------------------------------------------------------------------------
if($help){
  print($usage);
  exit(0);
}

printDebug("Verbose mode. Print debugging messages ...");

#----------------------------------------------------------------------------------------------------
# Check macro exists
#----------------------------------------------------------------------------------------------------
if ( ! -f $macro ){
  print "\n\n\n Error: No $macro exists in current directory. Stop \n\n\n";
  exit(0);
}
printDebug("Use macro: $macro");

#----------------------------------------------------------------------------------------------------
# Check number of arguments (should be 2)
#----------------------------------------------------------------------------------------------------
if ( $#ARGV + 1 != 2 ) {
  print "\n\n\n Error: You need 2 arguments. See below for help \n\n\n";
  print($usage);
  exit(0);
}
printDebug("Number of arguments is $#ARGV + 1");

#----------------------------------------------------------------------------------------------------
# Check validity of Begin/End runs
#----------------------------------------------------------------------------------------------------
my $beginRun = shift @ARGV ;
my $endRun   = shift @ARGV ;

if ( $beginRun > $endRun ){
  print "\n\n\n Error: End run is larger than begin run. (begin, end) = ($beginRun, $endRun). Stop \n\n\n";
  exit(0);
}

printDebug("Begin/End Run:   (begin, end) = ($beginRun, $endRun)");

#----------------------------------------------------------------------------------------------------
# Check output directory
#----------------------------------------------------------------------------------------------------
if ( ! -d $directory ) {
  print "\n\n\n Error: No output directory $directory exists. Stop \n\n\n";
  exit(0);
}

printDebug("Output directory for ROOT files: $directory");

#----------------------------------------------------------------------------------------------------
# Deformation flag
#----------------------------------------------------------------------------------------------------
my $nuclei = "spherical";
if ( $deformation =~ "kTRUE" ){
  $nuclei = "deformed"
}

#----------------------------------------------------------------------------------------------------
# Looping over all run number
#----------------------------------------------------------------------------------------------------
while ( $beginRun < $endRun ){
  $runNumber      = `printf "%04d" $beginRun` ;
  $outputFileName = "$directory/fastglaubermc_$system\_$energy" . "GeV_$type\_$nuclei\_run$runNumber.root";
  $submit         = "submit_condor.pl -v -s $macro $outputFileName $nevents $system $energy $type $deformation";
  printDebug("$submit");

  # Submit if "$run" flag is ON
  if($run){
    system("$submit");

    if($sleep>0){
      printDebug("Waiting for $sleep sec ...");
      system("sleep $sleep");
    }
  }

  $beginRun++;
}

#----------------------------------------------------------------------------------------------------
# Debug print
#----------------------------------------------------------------------------------------------------
sub printDebug {
  if(!$verbose){
    return;
  }

  my $arg = shift @_ ;
  print "DEBUG:   $arg\n";
  return;
}


