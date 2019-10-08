#! /usr/local/bin/perl -w


#====================================================================================================
# Generate embedding job submission xml file
#
# $Id: get_embedding_xml_rcf.pl,v 1.31 2019/10/04 08:59:15 starembd Exp $
# $Log: get_embedding_xml_rcf.pl,v $
# Revision 1.31  2019/10/04 08:59:15  starembd
# updated for bnl_condor_embed policy
#
# Revision 1.30  2019/08/23 06:18:08  starembd
# now daq files can be simlink
#
# Revision 1.29  2019/06/23 09:48:14  starembd
# added runnumber in all outpatch
#
# Revision 1.28  2018/10/20 08:46:02  starembd
# code cleanup for HFT embedding
#
# Revision 1.27  2018/10/20 07:05:53  starembd
# updated default daq tags dirs and daqEvents file
#
# Revision 1.26  2018/09/26 03:55:56  zhux
# adapted to HFT embedding
#
# Revision 1.25  2018/09/16 09:51:58  zhux
# updated to the latest setup at RCF.
#
# Revision 1.24  2018/09/16 09:29:19  zhux
# *** empty log message ***
#
# Revision 1.23  2015/04/15 06:36:40  zhux
# Added filesPerHour=0.1 in <job> descriptions, force to choose queue for long jobs
#
# Revision 1.22  2014/07/31 14:07:19  zhux
# Add simulator option for bfcMixer_TpcOnly.C
#
# Revision 1.21  2013/01/09 21:36:06  huck
# fix missing </job> for localStRoot=0
#
# Revision 1.20  2012/10/07 01:20:32  huck
# tagging get_embedding_xml_rcf.pl with r 1.20
#
# Revision 1.1  2012/10/07 01:10:30  huck
# added get_embedding_xml_rcf.pl for usage at RCF (localStRoot flag)
#
# Revision 1.19  2012/05/15 16:41:27  cpowell
# Added line to gzip log files
#
# Revision 1.18  2012/03/01 05:46:38  cpowell
# Corrected parameters for bfcMixer_TpcSvtSsd.C
#
# Revision 1.17  2012/01/31 00:25:11  cpowell
# Allow pt bin subfolder with option -ptbin. Generalize usage with event simulator.
#
# Revision 1.16  2011/08/04 19:50:04  cpowell
# Flag included to embed Pythia events. This excludes StPrepEmbedmaker from the chain and runs starsim before reconstruction.
#
# Revision 1.15  2011/02/25 17:53:19  hmasui
# Move csh files into eliza disk, remove list files. Fix xml syntax in local test.csh.
#
# Revision 1.14  2010/11/07 23:31:48  hmasui
# Added transverse vertex cut. Use eliza disk instead of HPSS. Determine local library path based on 32sl44
#
# Revision 1.13  2010/10/11 19:04:00  hmasui
# Added sync command before hsi
#
# Revision 1.12  2010/09/29 04:22:34  hmasui
# Dynamic directory creation for generator and temporary log files. Added chmod for HPSS directory
#
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
my $staroflDir    = "/star/data18"; # starreco home

my $force         = 0;                                              # Default is false (do not overwrite existing xml file)
my $production    = "P16id";                                        # Default production
my $library       = getLibrary($production);                        # Default library
my $outputXml     = getXmlFileName($production);                    # Default xml file name
my $requestNumber = 9999999999 ;                                    # Default request number
my $daqsDirectory = "$staroflDir/embedding/daq";                    # Default daq files directory
my $tagsDirectory = "$staroflDir/embedding/tags";                   # Default tag files directory
my $trgsetupName  = "AuAu_200_production_2014";                     # Default trigger setup name
my $trgForce      = 1;                                              # Default trigger setup name obtained from the above $trgsetupName, instead of get_pathFC.pl for a single daq file
my $bfcMixer      = "StRoot/macros/embedding/bfcMixer_Tpx.C";       # Default bfcMixer
my $zvertexCut    = 200 ;                                           # Default z-vertex cut
my $vrCut         = 100 ;                                           # Default vr cut
my $ptmin         = 0.0 ;                                           # Default pt lower cut off
my $ptmax         = 10.0 ;                                          # Default pt higher cut off
my $ymin          = -1.5 ;                                          # Default rapidity lower cut off
my $ymax          = 1.5 ;                                           # Default rapidity higher cut off
my $pid           = 8 ;                                             # Default geant id (is pi+)
my $multiplicity  = 1 ;                                             # Default multiplicity (is 1)
my $particleName  = "PiPlus" ;                                      # Default particle name (is pi+)
my $prodName      = $production ;                                   # Default prodName (4th last argument in the bfcMixer)
my $simulatorMode = 0 ;                                             # Default mode (OFF) for using starsim (no StPrepEmbedding) 			
my $zerobiasMode  = 0 ;                                             # Default mode (OFF) for using as many zerobias daq events as possible, set $EVENTS_START randomly in the xml file.
my $moretagsMode  = 0 ;                                             # Default mode (OFF) for using moretags file as input tags
my $kumacFile     = "StRoot/macros/embedding/pythiaTuneA_template.kumac"; # Kumac file for starsim 			
my $seed          = "StRoot/macros/embedding/get_random_seed";      # Random seed generator for starsim		
my $daqEvents     = "$staroflDir/embedding/daq";                    # File list for starsim with daq files and number of events for each file		
my $toscratch     = 1;                                              # Default copy daq and tags to $SCRATCH
my $ptbin         = 0 ;                                             # Default mode (OFF) for multiple pt hard bins for a single request 			
my $localStRoot   = 1;					  	    # default use local StRoot dir on

# Output path will be the following structure
# $starDisk/embedding/${TRGSETUPNAME}/${PARTICLENAME}_${FSET}_${REQUESTNUMBER}/${PRODUCTION}.${LIBRARY}/${YEAR}/${DAY}/${RUNNUMBER}
my $starDisk     = "/star/embed" ;                                 # Default star disk (is /star/embed)
my $logdisk      = "/star/embed" ;                                 # Default log disk  (is /star/embed)
$verbose          = 0 ;                                             # verbose flag (defalt is false)

my $maxFilesPerProcess = 1 ;       # 1 file per job
my $fileListSyntax     = "paths" ; # Read local file on disk
my $nevents            = 1000 ;    # Number of maximum events
my $ptOption           = "FlatPt"; # Default pt option
#my $generatorDir       = getGeneratorDirectory($production);
#my $logDirectory       = getLogDirectory($production);
my $generatorDir       = "" ;  # will be determined later by (production, particle name, request number)
my $logDirectory       = "" ;  # will be determined later by (production, particle name, request number)
my $libraryPath        = getLocalLibraryPath(); # Local library path depends on $CHOS

# Scripts
my $getPathFromFile    = "/afs/rhic.bnl.gov/star/packages/scripts/get_pathFC.pl";
my $getYearDayFromFile = "StRoot/macros/embedding/getYearDayFromFile.pl";
my $localTestScript    = "test.csh" ;  # Local test script for one input daq/tags file

# Some fixed strings in the xml file
my $fileBaseNameXml = "\${FILEBASENAME}";
my $jobIdXml        = "\${JOBID}";

GetOptions (
    'daq=s' => \$daqsDirectory,            # Daq file directory
    'force' => \$force,                    # Flag for overwrite
    'geantid=i' => \$pid,                  # Geantid
    'disk=s' => \$starDisk,                # Target star disk
    'logdisk=s' => \$logdisk,              # Target log disk
    'help' => \$help,
    'library=s' => \$library,              # Library
    'local' => \$local,                    # Make local test script
    'localStRoot=i' => \$localStRoot,      # localStRoot
    'mixer=s' => \$bfcMixer,               # bfcMixer
    'mode=s' => \$ptOption,                # pt option
    'mult=s' => \$multiplicity,            # Number of MC tracks per event
    'nevents=i' => \$nevents,              # Number of maximum events to be processed for ONE daq file
    'particlename=s' => \$particleName,    # Particle name
    'prodname=s' => \$prodName,            # prodName
    'production=s' => \$production,        # Production
    'ptmin=f' => \$ptmin,                  # Minimum pt cut
    'ptmax=f' => \$ptmax,                  # Maximum pt cut
    'requestnumber=i' => \$requestNumber,  # Request number
    'tag=s' => \$tagsDirectory,            # Set tags file directory
    'trg=s' => \$trgsetupName,             # Set trigger setup name
    'trgForce=i' => \$trgForce,            # Force to use trigger setup name 
    'triggerid=i' => \@triggerId,          # Set trigger id cut
    'vrcut=f' => \$vrCut,                  # Set vr cut
    'ymin=f' => \$ymin,                    # Minimum rapidity cut
    'ymax=f' => \$ymax,                    # Maximum rapidity cut
    'zvertex=f' => \$zvertexCut,           # Set z-vertex cut
    'simulator=i' => \$simulatorMode,      # Set Simulator mode	
    'zerobias=i' => \$zerobiasMode,        # Set Simulator/zerobias mode	
    'kumacfile=s' => \$kumacFile,          # Set Simulator input
    'moretags=i' => \$moretagsMode,        # Set moretags mode
    'seed=s' => \$seed,                    # Set Simulator random seed generator
    'daqevents=s' => \$daqEvents,          # Set Simulator daq file and event number list
    'toscratch=i' => \$toscratch,          # Set whether copy daq&tags files to $SCRATCH
    'ptbin=i' => \$ptbin,                  # Set pt bin description in output directory	
    'verbose' => \$verbose
);

#----------------------------------------------------------------------------------------------------
# Help messages
#----------------------------------------------------------------------------------------------------
my $usage = q(

  Usage: get_embedding_xml.pl [arguments]

  Current available arguments (in any order):

  -daq [daq file directory]    Set daq file directory (default is /home/starofl/embedding/$production)

  -disk [star disk]            Set target star disk (e.g. /star/data60) for the outputs
  -logdisk  [log disk]             Set target log disk on project (e.g. /star/data12) for the outputs
  -f                           Overwrite the existing xml file (default is false)
  -g (or --geantid) [GEANT id] GEANT3 id for input MC particle
  -h or --help                 Show this messages and exit

  -lib [library]               Set library. ex. -l SL08f (default will be detemined by $production)
  -local                       Make local test script to read one input daq/tag file. Output shell script is test.csh,
                               which will be not overwrited by default (use -f option to force overwriting)

  -localStRoot                 use local StRoot directory instead of $STAR/StRoot for bfcMixer, kumacFile, seed & getYearDayFromFile
  -mixer [bfcMixer]            Set bfcMixer macro (default is StRoot/macros/embeding/bfcMixer_TpcSvtSSd.C)
  -mode                        Set pt mode (default is FlatPt)

      ***  Current available modes are
      option               description
      FlatPt               Generate flat (pt,y) by 'phasespace' command
      strange              Generate flat (pt,y) with vertex smearing (Need appropriate chain option for vertex finder)
      Spectrum             Generate sloped pt. Slope is defined by temperature (default is 300MeV)

  -mult [Multiplicity]     Set multiplicity (default is 1 per event)

  -particle (or --particlename) [Particle name]   Set particle name (default is PiPlus)

  -prodname [prod. name]       Set prodName (the second last argument in the bfcMixer, default is P08ic)
  -production [production]     Set production. ex. -p P08ie (default is P08ic)

  -ptmin [Minimum pt cut]      Set minimum pt cut off (default is 0 GeV/c)
  -ptmax [Maximum pt cut]      Set maximum pt cut off (default is 10 GeV/c)
                               
  -r [request number]                   Set request number (default is 9999999999)
  -tag [tags file directory]            Set tag file directory (default is /home/starofl/embedding/$production)
  -trg [trigger setup name]             Set trigger setup name (default is 2007ProductionMinBias)
  -trgForce [force trigger setup name]  Set trigger setup name to -trg (default is obtained using get_pathFC.pl)
  -trig (or --triggerid) [trigger id's] Set trigger id cut (accept multiple trigger id's, see below)
      > get_embedding_xml.pl -trig 280001 -trig 290001

  -vrcut [vr cut value]                 Set vr (=sqrt{vx^2 + vy^2}) cut (default is 100cm)

  -ymin [Minimum rapidity cut]          Set minimum rapidity cut off (default is -1.5)
  -ymax [Maximum rapidity cut]          Set maximum rapidity cut off (default is 1.5)

  -z (or --zvertex) [max. z-vertex cut] Set z-vertex cut. The cut will be |vz| < cut

  -simulator [Simulator flag: 0 (off) or 1 (on) ] Set mode for using a simulator to generate events (kumac required, no StPrepEmbed)													

  -ptbin [ptbin flag: 0 (off) or 1 (on)] Set ptbin info in output folder

  -kumacfile [kumac file name]          Set directory and file name of kumac to generate events using a simulator

  -seed                                 Set location of random seed generator for simulator

  -daqevents [daq file directory]       Set daq file list containing number of events in each daq file (needed for starsim) 

  -toscratch [1 or 0]                   Set whether copy daq&tags files to local scratch area in each computing node (default is true) 

  -verbose                              Verbose flag to show debug messages

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
# prepend $STAR directory if localStRoot=0 
#----------------------------------------------------------------------------------------------------
if( ! $localStRoot ) {
  my $StDir = `echo -n \$STAR`;
  $bfcMixer = "$StDir/$bfcMixer";
  $kumacFile = "$StDir/$kumacFile";
  $seed = "$StDir/$seed";
  $getYearDayFromFile = "$StDir/$getYearDayFromFile";
}

# Common log/generator area under /project directory at RCF
#   The directories only used for the temporary storage for log files
#$EMLOGS = "$logdisk/embed/projects";
$EMLOGS = "${logdisk}/log";

#----------------------------------------------------------------------------------------------------
# Set generator/directory from production name, particle name and request number
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
my $mytrgsetupName    = getProdName($daqsDirectory, "daq");
if( $trgForce == 0 ) {
  $trgsetupName       = $mytrgsetupName;
}
$generatorDir         = getGeneratorDirectory($starDisk, $trgsetupName, $particleName, $requestNumber);
$tempDirectory        = getTempListDirectory($starDisk, $trgsetupName, $particleName, $requestNumber);
$tempLogDirectory     = getTempLogDirectory($starDisk, $trgsetupName, $particleName, $requestNumber, $production, $library);
#$logDirectory        = getLogDirectory($logdisk, $trgsetupName, $particleName, $requestNumber, $production, $library);
#$logDirectory        = getLogDirectory($production, $particleName, $requestNumber);
#my $tempLogDirectory = getTempLogDirectory($production, 1) ;

#----------------------------------------------------------------------------------------------------
# Make sure perl scripts exist
#----------------------------------------------------------------------------------------------------
checkFile($getYearDayFromFile);
checkFile($bfcMixer);
checkFile($kumacFile);
checkFile($seed);

#----------------------------------------------------------------------------------------------------
# Make sure tag/daq file and log file directory exists. If not, stop.
#----------------------------------------------------------------------------------------------------
checkDirectory($tagsDirectory, "tag");
checkDirectory($daqsDirectory, "daq");
checkDirectory($tempLogDirectory,  "tempLogDirectory");
checkDirectory($tempDirectory,  "tempDirectory");
#checkDirectory($tempLogDirectory,  "temporary log");
if ( $simulatorMode == 1 ) {
	checkFile($kumacFile);
	checkFile($seed);
	checkFile($daqEvents);
}

# No checks for logs anymore. will be created dynamically in the xml file
#checkDirectory($logDirectory,  "log");

$outputXml = getXmlFileName($production);

  print "\n";
  print "  Production:         $production\n";
  print "  Output xml file:    $outputXml\n";
  print "  Request number:     $requestNumber\n";
  print "  Use library:        $library \n";
  print "  Trigger setup name: $trgsetupName\n";
  print "  Local library path: $libraryPath\n";
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
print OUT "<job maxFilesPerProcess=\"$maxFilesPerProcess\" fileListSyntax=\"$fileListSyntax\" simulateSubmission=\"false\">\n";
#print OUT "<job maxFilesPerProcess=\"$maxFilesPerProcess\" fileListSyntax=\"$fileListSyntax\" filesPerHour=\"0.1\" simulateSubmission=\"false\">\n";
print OUT "\n";
print OUT "<command>\n";

#----------------------------------------------------------------------------------------------------
# Library
#----------------------------------------------------------------------------------------------------
printDebug("Add: starver $library ...");
print OUT "<!-- Load library -->\n";
print OUT "starver $library\n";
print OUT "rm -rf .sl*\n";
print OUT "echo 'Build local makers for $library ...'\n";
print OUT "cons\n";
print OUT "ls -la . \n";
print OUT "ls -la StRoot/ \n";
print OUT "ls -la .sl*/lib/ \n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Tags file, year/date, trigger setupname
#----------------------------------------------------------------------------------------------------
printDebug("Set tags file directory: $tagsDirectory ...");
print OUT "<!-- Set tags file directory -->\n";
print OUT "setenv EMBEDTAGDIR $tagsDirectory\n";
print OUT "\n";

# Added 'perl' command in front of scripts in order to be able to run in sl53
printDebug("Set year and day ...");
print OUT "<!-- Set year and day from filename -->\n";
print OUT "setenv EMYEAR `perl $getYearDayFromFile -y $fileBaseNameXml`\n";
print OUT "setenv EMDAY `perl $getYearDayFromFile -d $fileBaseNameXml`\n";
print OUT "setenv EMRUN `perl $getYearDayFromFile -r $fileBaseNameXml`\n";  #X.ZHU
print OUT "\n";

printDebug("Set log files area ...");
print OUT "<!-- Set log files area -->\n";
#print OUT "setenv EMLOGS " . getLogDirectory($logdisk, $trgsetupName, $particleName, $requestNumber, $production, $library) . "\n";
print OUT "setenv EMLOGS " . getCatalogDirectory($particleName, $logdisk, $requestNumber) . "\n";
print OUT "\n";


#printDebug("Set HPSS outputs/LOG path ...");
#my $hpssLogDir = "/nersc/projects/starofl/embedding/$trgsetupName/$particleName\_&FSET;_$requestNumber/$production.$library/\${EMYEAR}/\${EMDAY}";
#print OUT "<!-- Set HPSS outputs/LOG path -->\n";
#print OUT "setenv EMHPSS $hpssLogDir\n";

if( ! $trgForce ) {   #X.ZHU
printDebug("Set output path ...");
print OUT "<!-- Set output directory path -->\n";
print OUT "set RAWINPUT=\${FILEBASENAME}.daq\n";
print OUT "\n";
print OUT "setenv TRGSET  \`perl $getPathFromFile \$RAWINPUT\`\n";
print OUT "echo \"\$TRGSET\"\n";
print OUT "\n";
print OUT "setenv TRGNAME `echo \$TRGSET | sed \'s|^\\(.*\\)::.*|\\1|g'\`\n";
print OUT "echo \"\$TRGNAME\"\n";
print OUT "\n";
print OUT "setenv PATHNAME `echo \$TRGSET | sed \'s|.*daq\\(/.*\\)\$|\\1|g'\`\n";
print OUT "echo \"\$PATHNAME\"\n";
print OUT "\n";
}   #X.ZHU

my $outputDirectory = getOutputDirectory($starDisk, $trgsetupName, $particleName, $requestNumber, $production, $library, $trgForce);
my $listDirectory   = getListDirectory($starDisk, $trgsetupName, $particleName, $requestNumber, $production, $library, 1); #X.ZHU: list directory should be fixed at the time of job submission
print OUT "setenv EMOUTPUT $outputDirectory\n";
#print OUT "setenv EMLIST $listDirectory\n";  #X.ZHU
print OUT "setenv EMDIR $tempDirectory\n";
print OUT "\n";

print OUT "\n";
print OUT "<!-- Print out EMYEAR and EMDAY and EMLOGS -->\n";
print OUT "echo EMYEAR   : \$EMYEAR\n";
print OUT "echo EMDAY    : \$EMDAY\n";
print OUT "echo EMRUN    : \$EMRUN\n";   #X.ZHU
print OUT "echo EMLOGS   : \$EMLOGS\n";
print OUT "echo EMOUTPUT : \$EMOUTPUT\n";
#print OUT "echo EMLIST   : \$EMLIST\n"; #X.ZHU
#print OUT "echo EMHPSS : \$EMHPSS\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Set tags file
#----------------------------------------------------------------------------------------------------
my $daqFile = "\$INPUTFILE0"; #Define daq file
my $tagFile = "\$EMBEDTAGDIR/$fileBaseNameXml.tags.root"; #Define tag file
my $moretagFile = "\$EMBEDTAGDIR/$fileBaseNameXml.moretags.root"; #Define tag file  #X.ZHU
my $tagname = "tags" ;
if ( $moretagsMode == 1) {
   $tagname = "moretags";
}
my $tagDirScratch = "\$SCRATCH/tags"; 
my $tagFileMixer = "\$EMBEDTAGDIR/$fileBaseNameXml.$tagname.root"; 
if ( $toscratch ){
   $tagFileMixer = "$tagDirScratch/$fileBaseNameXml.$tagname.root"; 
   #$daqFile      = "\$SCRATCH/$fileBaseNameXml.daq";  #daq has been copied by scheduler, $INPUTFILE0 is redirected to the local daq file
}
printDebug("Set tags file: $tagFileMixer");

#----------------------------------------------------------------------------------------------------
# Set fzd file
#----------------------------------------------------------------------------------------------------
my $fzdFile = "$fileBaseNameXml.fzd"; #Define fzd file - it will be created, and lives in the current directory 
printDebug("Set fzd file: $fzdFile");

#----------------------------------------------------------------------------------------------------
# Copy tags to SCRATCH (daq has been copied by scheduler and $INPUTFILE0 is redirected to the local daq file)
#----------------------------------------------------------------------------------------------------
if( $toscratch ){
print OUT "<!-- Copy tags file to SCRATCH -->\n";
print OUT "mkdir -v $tagDirScratch\n";
#print OUT "cp -v \$INPUTFILE0 \$SCRATCH/\n";
print OUT "cp -v $tagFile $tagDirScratch/\n";
print OUT "if ( -e $moretagFile ) then\n"; #X.ZHU
print OUT "  cp -v $moretagFile $tagDirScratch/\n";  #X.ZHU
print OUT "endif\n";  #X.ZHU
print OUT "ls -lah \$SCRATCH/\n";
print OUT "ls -lah $tagDirScratch/ \n\n";
}

#----------------------------------------------------------------------------------------------------
# Set bfcMixer
#----------------------------------------------------------------------------------------------------
printDebug("Set bfcMixer: $bfcMixer ...");

print OUT "touch \$SCRATCH/\${FILEBASENAME}\_\${JOBID}.log\n";
print OUT "\n";

#---------------------------------------------------------------------------------------------------
# Run starsim with kumac file and random number generator
#---------------------------------------------------------------------------------------------------
if ( $simulatorMode == 1 ) {
	print OUT "\n";
	print OUT "<!-- Run starsim with kumac file -->\n";
	use File::Basename;
	print OUT "set daqevents=$daqEvents\n";
	print OUT "set kumac=$kumacFile\n";
	print OUT "set fzdFile=$fzdFile\n";
	print OUT "set random=`$seed`\n";
	print OUT "set seed=`echo \$random | awk '{print \$1}'`\n";
	print OUT "set ptmin=$ptmin\n";
	print OUT "set ptmax=$ptmax\n";
	print OUT "set nevents=`grep \$FILEBASENAME \$daqevents | awk '{print \$2}'`\n";
	print OUT "set nsimevents=\$nevents\n";
	print OUT "if ( \$nevents > $nevents ) then \n";
	print OUT "  set nsimevents = $nevents\n";
	print OUT "endif\n";
	print OUT "echo nevents = \$nevents, nsimevents = \$nsimevents, seed = \$seed, random = \$random, kumac = \$kumac, fzdFile=\$fzdFile, ptmin = \$ptmin, ptmax = \$ptmax\n";
#	print OUT "starsim -w 0 -b \$kumac \$fzdFile \$random \$nevents \$ptmin	\$ptmax\n\n";

	print OUT "root4star -b -q \$kumac\\\(\$nsimevents,\$seed,\\\"\$fzdFile\\\",\\\"$tagFileMixer\\\",$multiplicity,$pid,$ptmin,$ptmax,$ymin,$ymax\\\) &gt;&gt;&amp; \$SCRATCH/\${FILEBASENAME}\_\${JOBID}.log\n";
	print OUT "\n";

#for HFT+HIJING+ZB embedding only!
	if ( $zerobiasMode == 1 ) {
	   print OUT "echo DAQ FILE has \$nevents events\n";
	   print OUT "echo We need $nevents\n";
	   print OUT "if ( \$nevents > $nevents ) then \n";
	   print OUT "  set nmax = `echo \"\$nevents-$nevents-1\" | bc`\n";
	   print OUT "  set first = `shuf -i 1-\$nmax -n 1`\n";
	   print OUT "  set last  = `echo \"\${first}+$nevents\" | bc`\n";
	   print OUT "  echo We begin at \${first} and end at \${last}\n";
	   print OUT "else\n";
	   print OUT "  set first = 1\n";
	   print OUT "endif\n";
	   print OUT "setenv EVENTS_START \$first\n";
	   print OUT "\n";
	}

}

# Determine trigger string
my $triggerString = "0";
if ( @triggerId ){
  $triggerString = "triggers";
}


print OUT "<!-- Start job -->\n";
# Get bfcMixer
$execute_bfcMixer = get_bfcMixer($bfcMixer, $nevents, $daqFile, $tagFileMixer, $ptmin, $ptmax, $ymin, $ymax, $zvertexCut, $vrCut,
    $pid, $multiplicity, $triggerString, $prodName, $ptOption, $simulatorMode, $fzdFile) ;
print OUT "echo 'Executing $execute_bfcMixer ...'\n";
print OUT "\n";
print OUT "root4star -b &gt;&gt;&amp; \$SCRATCH/\${FILEBASENAME}\_\${JOBID}.log &lt;&lt;EOF\n";

# Put Trigger id's 
print OUT getTriggerVector(0, $triggerString, @triggerId) ;

print OUT "  .L $bfcMixer\n";
print OUT "  $execute_bfcMixer\n";
print OUT "  .q\n";
print OUT "EOF\n";
print OUT "ls -la .\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Make output directory
#----------------------------------------------------------------------------------------------------
print OUT "<!-- Make output and list directory (if they don't exist) -->\n";
print OUT "if ( ! -f \$EMOUTPUT ) then \n";
print OUT "  mkdir -pv \$EMOUTPUT\n";
#print OUT "  chmod -R ug+rw \$EMOUTPUT\n";
print OUT "endif\n";
print OUT "ls -l \$EMOUTPUT\n";
print OUT "\n";
#print OUT "if ( ! -f \$EMLIST ) then \n"; #X.ZHU
#print OUT "  mkdir -pv \$EMLIST\n";	   #X.ZHU
#print OUT "  chmod -R ug+rw \$EMLIST\n";
#print OUT "endif\n";			   #X.ZHU
#print OUT "ls -l \$EMLIST\n";		   #X.ZHU
print OUT "\n";
print OUT "if ( ! -f \$EMLOGS ) then \n";
print OUT "  mkdir -pv \$EMLOGS\n";
#print OUT "  chmod -R ug+rw \$EMLOGS\n";
print OUT "endif\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------
# Move LOG files and copy output ROOT files to star disk
#----------------------------------------------------------------------------------------------------
my $logFileName = "$fileBaseNameXml\_$jobIdXml.log";
my $logFileNameShort = "$fileBaseNameXml.log";
printDebug("Set logfilename: $logFileName ...");

my $errFileName = "$fileBaseNameXml.$jobIdXml.elog";
printDebug("Set errfilename: $errFileName ...");

print OUT "<!-- Move LOG files and csh to star disk, remove list files -->\n";
#print OUT "mv -v " . getTempLogDirectory($production, 0) . "/$jobIdXml.log \$EMOUTPUT/$logFileName\n";
#print OUT "mv -v " . getTempLogDirectory($production, 0) . "/$jobIdXml.elog \$EMOUTPUT/$errFileName\n";
#print OUT "gzip " . "\$EMOUTPUT/$logFileName\n";
#print OUT "gzip " . "\$EMOUTPUT/$errFileName\n";
print OUT "mv -v \$SCRATCH/$logFileName \$EMLOGS/$logFileNameShort\n"; 
#print OUT "gzip " . "\$EMLOGS/$logFileNameShort\n";  #X.ZHU

#print OUT "mv -v $tempLogDirectory/\${FILEBASENAME}_\${JOBID}.log \$EMLOGS/\n"; 
#print OUT "mv -v $tempLogDirectory/\${FILEBASENAME}_\${JOBID}.elog \$EMLOGS/\n"; 
#print OUT "mv -v $generatorDir/sched\$JOBID.csh \$EMLIST/\n";
#print OUT "rm -vf $generatorDir/sched\$JOBID.list\n";
#print OUT "mv -v \$EMDIR/LOG/\${FILEBASENAME}_\${JOBID}.log \$EMLIST/\n";  #X.ZHU
#print OUT "mv -v \$EMDIR/LOG/\${FILEBASENAME}_\${JOBID}.elog \$EMLIST/\n";  #X.ZHU
#print OUT "mv -v \$EMDIR/LIST/sched\$JOBID.csh \$EMLIST/\n"; #X.ZHU
#print OUT "rm -rfv \$EMDIR\n";
print OUT "\n";
#print OUT "<!-- Copy ROOT files to star disk -->\n";
#print OUT "cp -v *.root \$EMOUTPUT/\n";

#print OUT "<!-- New command to organize log files -->\n";
#print OUT "mkdir -p $logDirectory\n";
#print OUT "mv " . getTempLogDirectory($production, 0) . "/$jobIdXml.* $logDirectory/\n";
#print OUT "\n";

#----------------------------------------------------------------------------------------------------
# put log file in HPSS
#   - No longer put the outputs into HPSS
#----------------------------------------------------------------------------------------------------
#printDebug("Set archive log/root files in HPSS: $hpssLogDir ...");
#print OUT "<!-- Write buffers into disk -->\n";
#print OUT "sync\n";
#print OUT "\n";
#print OUT "<!-- Archive in HPSS -->\n";
#print OUT "set i = 0\n";
#print OUT "set ret = 1\n";
#print OUT "while (\$i &lt; 5 || \$ret != 0)\n";
#print OUT "  @ i++\n";
#print OUT "  hsi \"mkdir -p \$EMHPSS; prompt; cd \$EMHPSS; mput *.root; mput $logFileName; mput $errFileName; chmod ug+rw -R \$EMHPSS \"\n";
#print OUT "  set ret = \$status\n";
#print OUT "  if (\$ret == 0) then\n";
#print OUT "    break\n";
#print OUT "  endif\n";
#print OUT "  sleep 300\n";
#print OUT "end\n";

print OUT "</command>\n";
print OUT "\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Locations, log/elog, daq files, output csh/list files and local sand-box
#----------------------------------------------------------------------------------------------------
printDebug("Locations of log/elog, daq files, csh/list and local sand-box ...");

# Now, the directory for LOG files here is the temporary path to store the files.
# Files will be moved a new path determined by production, particle name, request number and FSET
print OUT "<!-- Define locations of ROOT files -->\n";
print OUT "<output fromScratch=\"st_*.root\" toURL=\"\$EMOUTPUT/\"/>\n";  #X.ZHU
if ( $simulatorMode == 1 ) { print OUT "<output fromScratch=\"st_*.fzd\" toURL=\"\$EMOUTPUT/\"/>\n"; }
print OUT "\n";
print OUT "<!-- Define locations of log/elog files -->\n";
#print OUT "<stdout URL=\"file:$tempLogDirectory/$jobIdXml.log\"/>\n";
#print OUT "<stderr URL=\"file:$tempLogDirectory/$jobIdXml.elog\"/>\n";
print OUT "<stdout URL=\"file:$tempLogDirectory/\$FILEBASENAME_\$JOBID.log\"/>\n";
print OUT "<stderr URL=\"file:$tempLogDirectory/\$FILEBASENAME_\$JOBID.elog\"/>\n";
print OUT "\n";
print OUT "<!-- Input daq files -->\n";
print OUT "<input URL=\"file:$daqsDirectory/st*\"/>\n";
print OUT "\n";
print OUT "<!-- csh/list files -->\n";
print OUT "<Generator>\n";
print OUT "  <Location>$generatorDir</Location>\n";
print OUT "</Generator>\n";
print OUT "\n";

if( $localStRoot ) {
  print OUT "<!-- Put any locally-compiled stuffs into a sand-box -->\n";
  print OUT "<SandBox installer=\"ZIP\">\n";
  print OUT "  <Package name=\"Localmakerlibs&FSET;\">\n";
#  print OUT "    <File>file:./$libraryPath/</File>\n";
  print OUT "    <File>file:./StRoot/</File>\n";
  print OUT "    <File>file:./pams/</File>\n";
  print OUT "    <File>file:./StarDb/</File>\n";
  print OUT "    <File>file:./Input/</File>\n";
  print OUT "  </Package>\n";
  print OUT "</SandBox>\n";
}
print OUT "</job>\n";
print OUT "\n";

#----------------------------------------------------------------------------------------------------
# Close xml file
#----------------------------------------------------------------------------------------------------
close(OUT);
printDebug("Close $outputXml ... (ok)\n\n");

#----------------------------------------------------------------------------------------------------
# Make local test shell script if "-local" is ON
#----------------------------------------------------------------------------------------------------
if($local){
  if ( -f $localTestScript ){
    if ( $force ){
      printDebug("Overwrite $localTestScript ...");
    }
    else{
      print "$localTestScript already existed. Don't overwrite current file. \n";
      print "Use -f option to force overwriting the file.\n";
      exit(0);
    }
  }

  # Find one daq file from the path, and also find the corredponding tags file
  my $daqOneFile  = `find $daqsDirectory -xtype f -iname "*daq" | head -n1`;
  chomp($daqOneFile);
  my $daqOneFileBaseName  = `basename $daqOneFile`;
  chomp($daqOneFileBaseName);

  # Make sure daq file exists
  if ( ! -f $daqOneFile ){
   	print "Can't find $daqOneFile. Stop\n";
   	exit(0);
  }
  printDebug("Found one daq file: $daqOneFile");

  my $tagsBaseName = `basename $daqOneFile | sed 's/\\.daq/\\.$tagname\\.root/g'`;
  chomp($tagsBaseName);
  my $tagsOneFile  = "$tagsDirectory/$tagsBaseName";

  # Make sure tags file exists
  if ( ! -f $tagsOneFile ){
		if ( $simulatorMode != 1 ){
    	print "Can't find $tagsOneFile. Stop\n";
    	exit(0);
		}
  }
  printDebug("Found one tags file: $tagsOneFile");

	#Make sure fzd file exists
  my $fzdOneFile = `basename $daqOneFile | sed 's/\\.daq/\\.fzd/g'`;
	chomp($fzdOneFile);
  printDebug("Create one fzd file: $fzdOneFile");
  
  open (LOCAL, ">$localTestScript") || die "can't open $localTestScript\n"; 
  print LOCAL "#!/bin/csh\n";
  print LOCAL "\n";
  print LOCAL "starver $library\n";
  print LOCAL "rm -rf .sl*\n";
  print LOCAL "echo 'Build local makers for $library ...'\n";
  print LOCAL "cons\n";
  print LOCAL "ls -la . \n";
  print LOCAL "ls -la StRoot/ \n";
  print LOCAL "ls -la .sl*/lib/ \n";
  print LOCAL "set daq  = \"$daqOneFile\"\n";
  print LOCAL "set tags = \"$tagsOneFile\"\n";
  print LOCAL "\n";

  # Get bfcMixer
  $execute_bfcMixer = get_bfcMixer($bfcMixer, 10, $daqOneFile, $tagsOneFile, $ptmin, $ptmax, $ymin, $ymax, $zvertexCut, $vrCut,
      $pid, $multiplicity, $triggerString, $prodName, $ptOption, $simulatorMode, $fzdOneFile);

# Run starsim
	if ( $simulatorMode == 1 ){
		use File::Basename;
		print LOCAL "set kumac=$kumacFile\n";
		print LOCAL "set fzd=$fzdOneFile\n";
		print LOCAL "set daqevents=$daqEvents\n";
		print LOCAL "set random=`$seed`\n";
		print LOCAL "set seed=`echo \$random | awk '{print \$1}'`\n";
		print LOCAL "set ptmin=$ptmin\n";
		print LOCAL "set ptmax=$ptmax\n";
		print LOCAL "set nevents=`grep \'$daqOneFileBaseName\' \$daqevents | awk '{print \$2}'`\n";
		print LOCAL "echo nevents = \$nevents, seed = \$seed, kumac = \$kumac, fzd=\$fzd, ptmin = \$ptmin, ptmax = \$ptmax\n";
		print LOCAL "\n";
#		print LOCAL "echo Running \"starsim -w 0 -b \$kumac \$fzd \$random \$nevents \$ptmin \$ptmax\"\n";
#		print LOCAL "starsim -w 0 -b \$kumac \$fzd \$random \$nevents \$ptmin	\$ptmax\n\n";
		print LOCAL "root4star -b -q \$kumac\\\(10,\$seed,\\\"\$fzd\\\",\\\"\$tags\\\",$multiplicity,$pid,$ptmin,$ptmax,$ymin,$ymax\\\)\n";
		print LOCAL "\n";
	}
 	print LOCAL "echo 'Executing $execute_bfcMixer ...'\n";
 	print LOCAL "\n";
 	print LOCAL "root4star -b <<EOF\n";
	
  # Put Trigger id's (Need to fix &lt; and &gt; by hand -> fixed)
  print LOCAL getTriggerVector(1, $triggerString, @triggerId) ;

  print LOCAL "  .L $bfcMixer\n";
  print LOCAL "  $execute_bfcMixer\n";
  print LOCAL "  .q\n";
  print LOCAL "EOF\n";
  close(LOCAL);

  # Make executable
  system("chmod u+x $localTestScript");

  printDebug("Close local test script $localTestScript ... (ok)\n\n");
}

# enf of script
#====================================================================================================

#----------------------------------------------------------------------------------------------------
# Get bfcMixer (either bfcMixer_TpcSvtSsd or bfcMixer_Tpx)
#----------------------------------------------------------------------------------------------------
sub get_bfcMixer {
  my $bfcMixer     = shift @_ ;
  my $nevents      = shift @_ ;
  my $daqfile      = shift @_ ;
  my $tagsfile     = shift @_ ;
  my $ptmin        = shift @_ ;
  my $ptmax        = shift @_ ;
  my $ymin         = shift @_ ;
  my $ymax         = shift @_ ;
  my $zvertexcut   = shift @_ ;
  my $vrcut        = shift @_ ;
  my $pid          = shift @_ ;
  my $multiplicity = shift @_ ;
  my $trigger      = shift @_ ;
  my $prodname     = shift @_ ;
  my $ptOption     = shift @_ ;
  my $simulator    = shift @_ ;
  my $fzdfile	   = shift @_ ;

  # Remove '.C' from macro
  my $bfcMixerFunction = `basename $bfcMixer`;
  chomp($bfcMixerFunction);
  $bfcMixerFunction =~ s/.C//g; # Remove .C
  printDebug("Prepare function: $bfcMixerFunction to execute in root4star");

  # Second last argument should be revisited. Currently, just put $production (Hiroshi)
  # Set executing bfcMixer's
  my $execute_bfcMixer = "" ;
  if ( $bfcMixer =~ /.*SvtSsd.C/ ){
    # bfcMixer_TpcSvtSsd.C needs two additional switches
    printDebug("SVT/SSD flags are added in the 2nd/3rd arguments");

    $execute_bfcMixer = "$bfcMixerFunction($nevents, 1, 1, \"$daqfile\", \"$tagsfile\", $ptmin, $ptmax, $ymin, $ymax, -$zvertexcut, $zvertexcut, $pid, $multiplicity, $trigger, \"$prodname\", \"$ptOption\");";
  }
  elsif ( $bfcMixer =~ /.*Tpx.C/  ){
    # bfcMixers with PYTHIA flag option (Tpx only) (CBPowell)
    printDebug("Starndard (without SVT/SSD) bfcMixer (Tpx only)");

    $execute_bfcMixer = "$bfcMixerFunction($nevents, \"$daqfile\", \"$tagsfile\", $ptmin, $ptmax, $ymin, $ymax, -$zvertexcut, $zvertexcut, $vrcut, $pid, $multiplicity, $trigger, \"$prodname\", \"$ptOption\", $simulator, \"$fzdfile\");";
  }
  elsif ( $bfcMixer =~ /.*Hft.C/  ){
    # bfcMixers with PYTHIA flag option (Hft)
    printDebug("Starndard (with PXL/IST/SST) bfcMixer (Hft)");

    $execute_bfcMixer = "$bfcMixerFunction($nevents, \"$daqfile\", \"$tagsfile\", $ptmin, $ptmax, $ymin, $ymax, -$zvertexcut, $zvertexcut, $vrcut, $pid, $multiplicity, $trigger, \"$prodname\", \"$ptOption\", $simulator, \"$fzdfile\");";
  }
  elsif ( $bfcMixer =~ /.*TpcOnly.C/  ){
    # bfcMixers with PYTHIA flag option (Tpc only) (X. Zhu)
    printDebug("Starndard (without SVT/SSD) bfcMixer (Tpc only)");

    $execute_bfcMixer = "$bfcMixerFunction($nevents, \"$daqfile\", \"$tagsfile\", $ptmin, $ptmax, $ymin, $ymax, -$zvertexcut, $zvertexcut, $vrcut, $pid, $multiplicity, $trigger, \"$prodname\", \"$ptOption\", $simulator, \"$fzdfile\");";
  }
	else {	
    # Other bfcMixers (TpcOnly or Tpx)
    printDebug("Starndard (without SVT/SSD) bfcMixer");

    $execute_bfcMixer = "$bfcMixerFunction($nevents, \"$daqfile\", \"$tagsfile\", $ptmin, $ptmax, $ymin, $ymax, -$zvertexcut, $zvertexcut, $vrcut, $pid, $multiplicity, $trigger, \"$prodname\", \"$ptOption\");";
  }
  printDebug("Executing bfcMixer looks: $execute_bfcMixer");

  return $execute_bfcMixer ;
}

#----------------------------------------------------------------------------------------------------
# Get trigger vectors
#----------------------------------------------------------------------------------------------------
sub getTriggerVector {
  my $flag          = shift @_ ;
  my $triggerString = shift @_ ;
  my @triggerArray = @_ ;

  $val = "";
  if ( @triggerArray ) {
    printDebug("Trigger id(s) requested");
    if ( $flag eq 0 ) {
      $val = $val . "  std::vector&lt;Int_t&gt; $triggerString;\n";
    }
    else {
      $val = $val . "  std::vector<Int_t> $triggerString;\n";
    }
    while ( @triggerArray ){
      $trigger = shift @triggerArray ;
      $val = $val . "  $triggerString.push_back($trigger);\n";
      printDebug("Push back trigger id = $trigger");
    }
  }

  return $val ;
}

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
# Get production directory
#----------------------------------------------------------------------------------------------------
sub getProductionDirectory {
  # flag controls to return
  # Enviromental variable "$EMLOGS" when flag == 0
  # Expanded strings of "$EMLOGS" when flag != 0
  my $production = shift @_ ;
  my $flag       = shift @_ ;

  if ( $flag == 0 ){
    return "\$EMLOGS/$production";
  }
  else{
    return "$EMLOGS/$production";
  }
}

#----------------------------------------------------------------------------------------------------
# Get common LOG/Generator path
#----------------------------------------------------------------------------------------------------
sub getEmbeddingProjectDirectory {
  # Make common directory for LOG/Generator from the production, particle name and request number
  # The directory structure will be : $EMLOGS/${production}/${particleName}_${requestNumber}
  my $production    = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $flag          = shift @_ ;
  my $productionDir = getProductionDirectory($production, $flag);
  return "$productionDir/$particleName\_$requestNumber";

}

#----------------------------------------------------------------------------------------------------
# Get generator directory
#----------------------------------------------------------------------------------------------------
sub getGeneratorDirectory {
  # Make generator directory from the production, particle name and request number
  # The directory structure will be : /${starDisk}/${production}/${particleName}_${requestNumber}/LIST
  # Do not use emvironmental variable here. Generator tag cannot recognize it.
  my $logdisk       = shift @_ ;
  my $trgsetupName  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  #my $dir           = getEmbeddingProjectDirectory($production, $particleName, $requestNumber, 1);
  my $dir           = getTempListDirectory($logdisk, $trgsetupName, $particleName, $requestNumber);
  my $target        = "$dir/LIST";

  # Check directory
  if ( -d $target ) {
    return "$target" ;
  }
  else{
    print "  Create generator directory : $target\n";
    system("mkdir -pv $target");

    #my $command = "chmod ug+rw -R $dir";
    #print "  Make target directory group readable, executing: $command \n";
    #system("$command");
  }

  return "$target" ;
}

#----------------------------------------------------------------------------------------------------
# Get temporary LOG directory
#----------------------------------------------------------------------------------------------------
sub getTempLogDirectory {
  # Make temporary LOG directory
#  my $production = shift @_ ;
#  my $flag       = shift @_ ;
#  my $target     = getProductionDirectory($production, $flag) . "/LOG";
#
#  # return here if $flag == 0
#  if ( $flag == 0 ){
#    return $target ;
#  }
#
  my $logdisk       = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $production    = shift @_ ;
  my $library       = shift @_ ;
  my $dir           = getTempListDirectory($logdisk, $trgsetupname, $particleName, $requestNumber);
  my $target        = "$dir/LOG";

  # Check directory
  if ( -d $target ) {
    return "$target" ;
  }
  else{
    print "  Create temporary log directory : $target\n";
    system("mkdir -pv $target");

    #my $command = "chmod ug+rw -R $target";
    #print "  Make target directory group readable, executing: $command \n";
    #system("$command");
  }
  return $target
}
#----------------------------------------------------------------------------------------------------
# Get temporary list directory
#----------------------------------------------------------------------------------------------------
sub getTempListDirectory {
  my $logdisk       = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $target        = "$logdisk/embedding/$trgsetupname/$particleName\_$requestNumber";

  # Check directory
  if ( -d $target ) {
    return "$target" ;
  }
  else{
    print "  Create temporary log directory : $target\n";
    system("mkdir -pv $target");

    #my $command = "chmod ug+rw -R $target";
    #print "  Make target directory group readable, executing: $command \n";
    #system("$command");
  }

  return $target;
}
#----------------------------------------------------------------------------------------------------
# Get LOG directory
#----------------------------------------------------------------------------------------------------
sub getLogDirectory {
#  # Make log directory from the production, particle name, request number and FSET
#  # The directory structure will be : $EMLOGS/${production}/${particleName}_${requestNumber}/LOG/&FSET;
#  my $production    = shift @_ ;
#  my $particleName  = shift @_ ;
#  my $requestNumber = shift @_ ;
#  my $dir           = getEmbeddingProjectDirectory($production, $particleName, $requestNumber, 0);
#  return "$dir/LOG/&FSET;";
  my $logdisk       = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $production    = shift @_ ;
  my $library       = shift @_ ;
  return "$logdisk/embedding/$trgsetupname/$particleName\_&FSET;_$requestNumber/$production.$library/\$EMYEAR/\$EMDAY";
}
#----------------------------------------------------------------------------------------------------
# Get log catalog directory
#----------------------------------------------------------------------------------------------------
sub getCatalogDirectory {
  my $particleName  = shift @_ ;
  my $logdisk       = shift @_ ;
  my $requestNumber = shift @_ ;
  my $target        = "$logdisk/log/$particleName\_&FSET;_$requestNumber/log"; 
  return $target;

}
#----------------------------------------------------------------------------------------------------
# Get output directory
#----------------------------------------------------------------------------------------------------
sub getOutputDirectory {
  my $stardisk     = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $production    = shift @_ ;
  my $library       = shift @_ ;
  my $flag          = shift @_ ;
  
  if ( $flag == 0 ){
    $target = "$stardisk/embedding/\${TRGNAME}/$particleName\_&FSET;_$requestNumber/$production.$library\${PATHNAME}";	
  }
  else {
    $target = "$stardisk/embedding/$trgsetupname/$particleName\_&FSET;_$requestNumber/$production.$library/\$EMYEAR/\$EMDAY/\$EMRUN"; #X.ZHU: should avoid dependence on pathFC.pl
  }
  return $target
}

#----------------------------------------------------------------------------------------------------
# Get output directory (with pt hard bin)
#----------------------------------------------------------------------------------------------------
sub getOutputDirectoryPt {
  my $stardisk     = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $production    = shift @_ ;
  my $library       = shift @_ ;
  my $ptmin 	      = shift @_ ;
  my $ptmax 	      = shift @_ ;
  return "$stardisk/embed/$trgsetupname/$particleName\_&FSET;_$requestNumber/$production.$library/\$EMYEAR/\$EMDAY/\$EMRUN/Pt\_$ptmin\_$ptmax";
}
#----------------------------------------------------------------------------------------------------
# Get list directory
#----------------------------------------------------------------------------------------------------
sub getListDirectory {
  my $stardisk     = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $production    = shift @_ ;
  my $library       = shift @_ ;
  my $flag          = shift @_ ;

  if ( $flag == 0 ){
    $target = "$stardisk/embedding/\${TRGNAME}/$particleName\_$requestNumber/FSET&FSET;_$production.$library\_\$EMYEAR";
  }
  else {
    $target = "$stardisk/embedding/$trgsetupname/$particleName\_$requestNumber/FSET&FSET;_$production.$library\_\$EMYEAR";
  }
  return $target
}
#----------------------------------------------------------------------------------------------------
# Get list directory (with pt hard bin)
#----------------------------------------------------------------------------------------------------
sub getListDirectoryPt {
  my $stardisk     = shift @_ ;
  my $trgsetupname  = shift @_ ;
  my $particleName  = shift @_ ;
  my $requestNumber = shift @_ ;
  my $production    = shift @_ ;
  my $library       = shift @_ ;
  my $ptmin 	      = shift @_ ;
  my $ptmax 	      = shift @_ ;
  return "$stardisk/embed/$trgsetupname/$particleName\_$requestNumber/FSET&FSET;_$production.$library\_\$EMYEAR/Pt\_$ptmin\_$ptmax";
}

#----------------------------------------------------------------------------------------------------
# Get local library path
#----------------------------------------------------------------------------------------------------
sub getLocalLibraryPath {
  #  $CHOS       path
  # 32sl44 -->  .sl44_gcc346
  # sl53   -->  .sl53_gcc432
  #my $chos = `echo \$CHOS`;
  #if ( $chos =~ "32sl44" ){
  #  return ".sl44_gcc346";
  #}
  #elsif ( $chos =~ "sl53" ){
  #  return ".sl53_gcc432";
  #}
  #else{
  #  print "Unknown OS : $chos. Set the sl44 path\n";
  #  return ".sl44_gcc346";
  #}
  return ".sl73_gcc485";
}

#----------------------------------------------------------------------------------------------------
# Check production name of daq files
#----------------------------------------------------------------------------------------------------
sub getProdName {
  my $dir       = shift @_ ;
  my $ext       = shift @_ ;

  if( ! -d $dir){
    print "\n";
    print "    Error: No directory $dir exists. Stop. \n";
    print "    Make sure you've put the correct path for $dir dir. \n";
    print "\n";
    exit(0);
  }

  my $oneFile  = `find $dir -xtype f -iname "*$ext" | head -n1`;
  chomp($oneFile);
  my $oneFileBaseName  = `basename $oneFile`;
  chomp($oneFileBaseName);
  my $trgset=`perl $getPathFromFile $oneFileBaseName`;
  my @values=split(':', $trgset);
  my $trgname=$values[0];
  if($verbose){
    printDebug("OK: (trigname) $trgname");
  }
  return $trgname;
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

