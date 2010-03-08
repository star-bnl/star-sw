#! /usr/local/bin/perl -w

#====================================================================================================
# Generate embedding job submission xml file
#----------------------------------------------------------------------------------------------------

use Getopt::Long;

my $man = 0;
my $help = 0;
my $date = `date` ;
chomp($date);

#----------------------------------------------------------------------------------------------------
# Available options
#----------------------------------------------------------------------------------------------------
# Default parameters
my $staroflDir    = "/home/starofl";

my $force         = 0;                                     # Default is false (do not overwrite existing xml file)
my $production    = "P08ic";                               # Default production
my $library       = getLibrary($production);               # Default library
my $outputXml     = getXmlFileName($production);           # Default xml file name
my $requestNumber = 9999999999 ;                           # Default request number
my $daqsDirectory = "$staroflDir/embedding/$production";   # Default daq files directory
my $tagsDirectory = "$staroflDir/embedding/$production";   # Default tag files directory
my $trgsetupName  = "2007ProductionMinBias";               # Default trigger setup name
my $bfcMixer      = "bfcMixer_TpcSvtSsd.C";                # Default bfcMixer
$verbose          = 0 ;                                    # verbose flag (defalt is false)

my $maxFilesPerProcess = 1 ;       # 1 file per job
my $fileListSyntax     = "paths" ; # Read local file on disk
my $nevents            = 1000 ;    # Number of maximum events
my $ptOption           = "FlatPt"; # Default pt option
my $generatorDir       = getGeneratorDirectory($production);
my $logDirectory       = getLogDirectory($production);
my $libraryPath        = ".sl44_gcc346";      # Default library path

# Scripts
my $getYearFromFile = "$staroflDir/aarose/getYearFomrFile.pl";
my $getDayFromFile  = "$staroflDir/aarose/getDayFomrFile.pl";

GetOptions (
    'daq=s' => \$daqsDirectory,            # Daq file directory
    'force' => \$force,                    # Flag for overwrite
    'help' => \$help,
    'library=s' => \$library,              # Library
    'log=s' => \$logDirectory,             # Log file directory
    'mixer=s' => \$bfcMixer,               # bfcMixer
    'production=s' => \$production,        # Production
    'pt=s' => \$ptOption,                  # pt option
    'requestnumber=n' => \$requestNumber,  # Request number
    'tag=s' => \$tagsDirectory,            # Set tags file directory
    'trg=s' => \$trgsetupName,             # Set trigger setup name
    'verbose' => \$verbose
);

#----------------------------------------------------------------------------------------------------
# Help messages
#----------------------------------------------------------------------------------------------------
my $usage = q(

  Usage: get_embedding_xml.pl [arguments]

  Current available arguments (in any order):

  -daq [daq file directory]    Set daq file directory (default is /home/starofl/embedding/$production)

  -f                           Overwrite the existing xml file (default is false)
  -h or --help                 Show this messages and exit

  -lib [library]               Set library. ex. -l SL08f (default will be detemined by $production)
  -log [log file directory]    Set log file directory (default is /project/projectdirs/star/embedding/$production/LOG/)
  -m (or --mixer) [bfcMixer]   Set bfcMixer macro (default is bfcMixer_TpcSvtSSd.C)
  -pro [production]            Set production. ex. -p P08ie (default is P08ic)
  -pt [pt option]              Set pt option (default is FlatPt)
                               (will add current available pt options below soon, hiroshi)

  -r [request number]          Set request number (default is 9999999999)
  -tag [tags file directory]   Set tag file directory (default is /home/starofl/embedding/$production)
  -trg [trigger setup name]    Set trigger setup name (default is 2007ProductionMinBias)

  -v or --verbose              Verbose flag to show debug messages

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
printDebug("Verbose mode. Print debugging messages ...");

#----------------------------------------------------------------------------------------------------
# Set generator/directory from production name
#----------------------------------------------------------------------------------------------------
$generatorDir = getGeneratorDirectory($production);
$logDirectory = getLogDirectory($production);

#----------------------------------------------------------------------------------------------------
# Make sure tag/daq file and log file directory exists. If not, stop.
#----------------------------------------------------------------------------------------------------
checkDirectory($tagsDirectory, "tag");
checkDirectory($daqsDirectory, "daq");
checkDirectory($generatorDir,  "generator");
checkDirectory($logDirectory,  "log");

$outputXml = getXmlFileName($production);

  print "\n";
  print "  Production:         $production\n";
  print "  Output xml file:    $outputXml\n";
  print "  Request number:     $requestNumber\n";
  print "  Use library:        $library \n";
  print "  Trigger setup name: $trgsetupName\n";
  print "\n";

#----------------------------------------------------------------------------------------------------
# Open xml file
#  - Check if the identical file has alreday exist and stop script if you find it
#  - Force to overwrite if you put -f option
#----------------------------------------------------------------------------------------------------
if(!$force){
  # Check if xml file has already existed
  if ( -f $outputXml ){
    print "\n";
    print "    $outputXml exists in your working directory. Stop.\n";
    print "    Add -f option if you want to overwrite the current xml file.\n";
    print "\n";
    print "\n";
    exit(0);
  }
}
else{
  printDebug("Force to overwrite: $outputXml");
}

open (OUT, ">$outputXml") || die "can't open $outputXml \n";
printDebug("Open $outputXml ... (ok)");

#----------------------------------------------------------------------------------------------------
# Initialize xml file
#----------------------------------------------------------------------------------------------------
print OUT "<!-- Generated by $0 on $date -->\n";
print OUT "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
print OUT "<job maxFilesPerProcess=\"$maxFilesPerProcess\" fileListSyntax=\"$fileListSyntax\">\n";
print OUT "\n";
print OUT "<command>\n";

#----------------------------------------------------------------------------------------------------
# Library
#----------------------------------------------------------------------------------------------------
printDebug("Add: starver $library ...");
print OUT "<!-- Load library -->\n";
print OUT "starver $library\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Tags file, year/date, trigger setupname
#----------------------------------------------------------------------------------------------------
printDebug("Set tags file directory: $tagsDirectory ...");
print OUT "<!-- Set tags file directory -->\n";
print OUT "setenv EMBEDTAGDIR $tagsDirectory\n";
print OUT "\n";

printDebug("Set year and day ...");
print OUT "<!-- Set year and day from filename -->\n";
print OUT "setenv EMYEAR `$getYearFromFile \${FILEBASENAME}`\n";
print OUT "setenv EMDAY `$getDayFromFile \${FILEBASENAME}`\n";
print OUT "\n";

printDebug("Set trigger setup name: $trgsetupName ...");
print OUT "<!-- Set trigger setup name -->\n";
print OUT "setenv TRG $trgsetupName\n";
print OUT "\n";
print OUT "<!-- Print out EMYEAR and EMDAY -->\n";
print OUT "echo \$EMYEAR\n";
print OUT "echo \$EMDAY\n";
print OUT "\n";
print OUT "<!-- Start job -->\n";

#----------------------------------------------------------------------------------------------------
# Second last argument should be revisited. Currently, just put $production (Hiroshi)
#----------------------------------------------------------------------------------------------------
$tagFile = "\$EMBEDTAGDIR/\${FILEBASENAME}.tags.root"; #Define tag file
printDebug("Set tags file: $tagFile, bfcMixer: $bfcMixer ...");

print OUT "root4star -b -q $bfcMixer\\($nevents,\\\"\$INPUTFILE0\\\",\\\"$tagFile\\\",&PTLOW;,&PTHIGH;,&ETALOW;,&ETAHIGH;,&PID;,&MULT;,\\\"$production\\\",\\\"$ptOption\\\"\\\)\n";
print OUT "ls -la .\n";

#----------------------------------------------------------------------------------------------------
# Copy log/error files in the working directory
#----------------------------------------------------------------------------------------------------
my $logFileName = "\${FILEBASENAME}.\$JOBID.log";
printDebug("Set logfilename: $logFileName ...");

my $errFileName = "\${FILEBASENAME}.\$JOBID.elog";
printDebug("Set errfilename: $errFileName ...");

print OUT "cp $logDirectory/\$JOBID.log $logFileName\n";
print OUT "cp $logDirectory/\$JOBID.elog $errFileName\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# put log file in HPSS
#----------------------------------------------------------------------------------------------------
my $hpssLogDir = "/nersc/projects/starofl/embedding/\$TRG/&PNAME;_&FSET;_$requestNumber/$production.$library/\${EMYEAR}/\${EMDAY}";
printDebug("Set archive log/root files in HPSS: $hpssLogDir ...");
print OUT "<!-- Archive in HPSS -->\n";
print OUT "hsi \"mkdir -p $hpssLogDir; prompt; cd $hpssLogDir; mput *.root; mput $logFileName; mput $errFileName\"\n";
print OUT "\n";
print OUT "</command>\n";
print OUT "\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Locations, log/elog, daq files, output csh/list files and local sand-box
#----------------------------------------------------------------------------------------------------
printDebug("Locations of log/elog, daq files, csh/list and local sand-box ...");
print OUT "<!-- Define locations of log/elog files -->\n";
print OUT "<stdout URL=\"file:$logDirectory/\$JOBID.log\"/>\n";
print OUT "<stderr URL=\"file:$logDirectory/\$JOBID.elog\"/>\n";
print OUT "\n";
print OUT "<!-- Input daq files -->\n";
print OUT "<input URL=\"file:$daqsDirectory/st*\"/>\n";
print OUT "\n";
print OUT "<!-- csh/list files -->\n";
print OUT "<Generator>\n";
print OUT "  <Location>$generatorDir</Location>\n";
print OUT "</Generator>\n";
print OUT "\n";
print OUT "<!-- Put any locally-compiled stuffs into a sand-box -->\n";
print OUT "<SandBox installer=\"ZIP\">\n";
print OUT "  <Package name=\"Localmakerlibs\">\n";
print OUT "    <File>file:./$libraryPath/</File>\n";
print OUT "    <File>file:./StRoot/</File>\n";
print OUT "    <File>file:./asps/</File>\n";
print OUT "  </Package>\n";
print OUT "</SandBox>\n";
print OUT "</job>\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Close xml file
#----------------------------------------------------------------------------------------------------
close(OUT);
printDebug("Close $outputXml ... (ok)");

# enf of script
#====================================================================================================

#----------------------------------------------------------------------------------------------------
# Output filename (xml)
#----------------------------------------------------------------------------------------------------
sub getXmlFileName {
  my $production = shift @_ ;
  return "embed_template_$production.xml";
}

#----------------------------------------------------------------------------------------------------
# Library name
#   convert production name (ex. P08ic --> SL08c)
#----------------------------------------------------------------------------------------------------
sub getLibrary {
  my $production = shift @_ ;
  $production =~ s/^P/SL/;
  $production =~ s/SL(\d+)i([a-z])/SL$1$2/;
  return $production ;
}

#----------------------------------------------------------------------------------------------------
# Get generator directory
#----------------------------------------------------------------------------------------------------
sub getGeneratorDirectory {
  my $production = shift @_ ;
  return "/project/projectdirs/star/embedding/$production";
}

#----------------------------------------------------------------------------------------------------
# Get LOG directory
#----------------------------------------------------------------------------------------------------
sub getLogDirectory {
  my $production = shift @_ ;
  my $generator  = getGeneratorDirectory($production);
  my $log        = "$generator/LOG";
  return $log;
};

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

