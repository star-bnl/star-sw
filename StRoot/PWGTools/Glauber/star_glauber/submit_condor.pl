#! /usr/bin/perl
use Getopt::Long;

# Default arguments
$verbose     = 0 ;
my $help   = 0 ;
my $log    = "LOG_Scan";
my $queue  = 0 ; # batch queue flag
my $submit = 0 ;

GetOptions (
    'verbose' => \$verbose,    # Debug flag
    'help' => \$help ,         # Help messages
    'log=s'  => \$log,         # Set log directory (default is $PWD/LOG)
    'queue' => \$queue,        # Use "long" queue (default is cas)
    'submit' => \$submit       # submit jobs
);

#----------------------------------------------------------------------------------------------------
# Help messages
#----------------------------------------------------------------------------------------------------
my $usage = q(

  Usage: submit_condor.pl [-d] [-h] [-l log directory] [-s]  'command'  'arguments ...'

  Submit 'command' into the condor batch queue

  Current available arguments (in any order):

  -h or --help                         Show this messages and exit
  -l or --log [log file directory]     Set log directory (default is $PWD/LOG)
     -l SCRATCH will set the log directory in $SCRATCH

  -q or --queue                        Use long queue (default is short queue, cas)
  -s or --submit                       Submit jobs
  -v or --verbose                      Print debug messages

  In order to submit job, you need add '-s' option like
  submit_condor.pl -s [your command] [your arguments]

  Otherwise the submit_condor.pl will just return the submission strings



);

#----------------------------------------------------------------------------------------------------
# Print help messages and exit
#----------------------------------------------------------------------------------------------------
if( $help )
{
  print($usage);
  exit(0);
}

#----------------------------------------------------------------------------------------------------
# Debugging messages
#----------------------------------------------------------------------------------------------------
printDebug("\n\nVerbose mode. Print debugging messages ...");

my $currentDir = `pwd` ;
chomp($currentDir);
printDebug("Current directory: $currentDir");

#----------------------------------------------------------------------------------------------------
# Execute command, make sure it exists
#----------------------------------------------------------------------------------------------------
my $nARGV = $#ARGV + 1 ;
printDebug("# of arguments :   $nARGV");
for(my $i=0; $i<=$#ARGV; $i++) {
  printDebug("The argument $i+1: $ARGV[$i]");
}

if ( $nARGV == 0 ){
  print "";
  print " No argument. See usage below";
  print($usage);
  exit(0);
}

#----------------------------------------------------------------------------------------------------
# Execute command, and make sure it exists
#----------------------------------------------------------------------------------------------------
my $execLocal   = shift @ARGV ;
my $execCommand = "$currentDir/$execLocal" ;
checkFile($execCommand);
printDebug("Execute command:  $execCommand");

#----------------------------------------------------------------------------------------------------
# Make log file strings
#----------------------------------------------------------------------------------------------------
$execHeader = `echo $execLocal | sed 's/\.csh//g' | sed 's/\.pl//g'` ;
chomp($execHeader);

my $logHeader = "$execHeader";
my @arguments = ("");

if( $#ARGV + 1 != 0 ){ # Arguments > 0
  # Remove path, and ".root"
  for(my $i = 0; $i < $#ARGV + 1; $i++){
    # For job submission arguments
    @arguments = @ARGV ;

    # For log files (remove .root, .csh .list)
    $tmp[$i] = `basename $ARGV[$i] | sed 's/\\.root//g' | sed 's/\\.csh//g' | sed 's/\\.list//g' | sed 's/\\./_/g'` ;
    chomp($tmp[$i]);
  }

  $logHeader = "$execHeader\_@tmp";
  # Remove space between arguments
  $logHeader = `echo $logHeader | sed 's/ /_/g'` ;
  chomp($logHeader);
}

#----------------------------------------------------------------------------------------------------
# Make log directory to full path
#----------------------------------------------------------------------------------------------------
my $logFullPath = $log ;

if ( $logFullPath =~ /SCRATCH/ ){
  # Do not check directory for $SCRATCH
  $logFullPath = "\$SCRATCH";
  printDebug("Use \$SCRATCH directory for log files ...");
}
elsif ( $logFullPath !~ /^\// ) {
  # Supposed to be the path in the current directory
  $logFullPath = "$currentDir/$log" ;
  checkDirectory($logFullPath, "log");
}

my $outFile = "$logFullPath/$logHeader.out";
my $logFile = "$logFullPath/$logHeader.log";
my $errFile = "$logFullPath/$logHeader.err";
printDebug("Out file :  $outFile");
printDebug("Log file :  $logFile");
printDebug("Err file :  $errFile");

#----------------------------------------------------------------------------------------------------
# Delete previous log if they are found
#----------------------------------------------------------------------------------------------------
if ( -e $logFile ){
  system("rm -v $logFile");
}

if ( -e $errFile ){
  system("rm -v $errFile");
}

#----------------------------------------------------------------------------------------------------
# Set queue
#----------------------------------------------------------------------------------------------------
my $batchQueue = "cas";
if( $queue ){
  $batchQueue = "long";
}
printDebug("Use queue = $batchQueue");

#----------------------------------------------------------------------------------------------------
# Set submission command
#----------------------------------------------------------------------------------------------------
$requirements = '(CPU_Type != "crs") && (CPU_Experiment == "star") && CPU_Experiment != "lsst" && CPU_Experiment != "atlas"';
$usermail = "chenlz\@rcf.rhic.bnl.gov";

#print "-------------------------- condor_submit input list --------------------------\n" ;
#print "\n" ;
#print "Universe        = vanilla\n" ;
#print "Notification    = Error\n" ;
#print "Executable      = $execCommand\n";
#print "Arguments       = @arguments\n";
#print "Initialdir      = $currentDir\n" ;
#print "Output          = $outFile\n" ;
#print "Error           = $errFile\n" ;
#print "Log             = $logFile\n" ;
#print "Requirements    = $requirements\n" ;
#print "GetEnv          = True\n" ;
#print "Notify_user     = $usermail\n" ;
#print "Priority        = +10\n" ;
#print "+Experiment     = \"star\"\n" ;
#print "+Job_Type       = \"$batchQueue\"\n" ;
#print "PeriodicRemove  = (JobStatus == 2 && (CurrentTime - JobCurrentStartDate > (54000)) && ((RemoteUserCpu+RemoteSysCpu)/(CurrentTime-JobCurrentStartDate) < 0.10))\n" ;
#print "Queue\n";
#print "\n" ;
#print "------------------------------------------------------------------------------\n" ;

if ( $submit ) {
  # Make temporary xml file to submit job. tmpSubmitJob.xml
  my $xmlFile = "tmpSubmitJob.xml"; # will be deleted right after job submission
  open (OUT, ">$xmlFile") || die "can't open $xmlFile\n";
  print OUT "Universe        = vanilla\n" ;
  print OUT "Notification    = Error\n" ;
  print OUT "Executable      = $execCommand\n";
  print OUT "Arguments       = @arguments\n";
  print OUT "Initialdir      = $currentDir\n" ;
  print OUT "Output          = $outFile\n" ;
  print OUT "Error           = $errFile\n" ;
  print OUT "Log             = $logFile\n" ;
  print OUT "Requirements    = $requirements\n" ;
  print OUT "GetEnv          = True\n" ;
  print OUT "Notify_user     = $usermail\n" ;
  print OUT "Priority        = +10\n" ;
  print OUT "+Experiment     = \"star\"\n" ;
  print OUT "+Job_Type       = \"$batchQueue\"\n" ;
  print OUT "PeriodicRemove  = (JobStatus == 2 && (CurrentTime - JobCurrentStartDate > (54000)) && ((RemoteUserCpu+RemoteSysCpu)/(CurrentTime-JobCurrentStartDate) < 0.10))\n" ;
  print OUT "Queue\n";
  close(OUT);
  system("condor_submit $xmlFile");
  system("rm $xmlFile");
}

#----------------------------------------------------------------------------------------------------
# Check file exists
#----------------------------------------------------------------------------------------------------
sub checkFile {
  my $file = shift @_ ;
  if( -f $file ){
    if($verbose){
      printDebug("OK: $file");
    }

    return;
  }

  print "\n";
  print "    Error: No $file exists. Stop. \n";
  print "    Make sure you've put the correct path for $file file. \n";
  print "\n";
  exit(0);
}

#----------------------------------------------------------------------------------------------------
# Check directory exists
#----------------------------------------------------------------------------------------------------
sub checkDirectory {
  my $directory = shift @_ ;
  my $file      = shift @_ ;
  if( -d $directory ){
    if($verbose){
      printDebug("OK: ($file) $directory");
    }

    return;
  }

  print "\n";
  print "    Error: No $directory exists. Stop. \n";
  print "    Make sure you've put the correct path for $file file. \n";
  print "\n";
  exit(0);
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

