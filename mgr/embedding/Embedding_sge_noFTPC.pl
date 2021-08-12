#! /usr/bin/perl

# Steering routine for embedding on PDSF 
#
# Peter Jacobs and Bum Choi, 12/16/00
# Version 4 Eric Hjort, Sept. 2001
#
# The chain is set up such that the output filename of each process is the 
# input filename of the following process
#
# Note: the script extracts the star production/library version from
# the path where the tags.root files are located.
# The expected form is .../(reco|tags)/P(version/...
# softlinks are allowed as the targets are resolved.
#
# Usage: scriptname 
#    <setup file> <hpss mode> <daq dir> <tags dir> <disk vault> <daq type> [nevent]
#
#---------------------------------------------------------
my $code_dir = "/home/starofl/embedding/P06ib";
#---------------------------------------------------------

use lib "/home/starofl/embedding/P06ib/EmbeddingLib_v4_noFTPC";

use EmbeddingUtilities;

use Process_object;
use Chain_object;
use File::Basename;
use Cwd;

use strict;

#---------------------------------------------------------
my ($SetupFile, $Mode, $DaqDir, $TagDir, $DiskVault, $daqType, $NEvents) = GetArgs();

$DaqDir =~ /starprod\/daq\/\d+\/(\w+)\/(\w+)/;
my $daqField = $2;

my ($setup_name, $setup_path, $setup_suffix) = fileparse( $SetupFile, '\.setup');

my $jobFile = $setup_name.$DiskVault.$daqField;
open(LIST,"../Common/lists/$jobFile.list") or die "Error: $jobFile.list $!\n";
my @labels;
while(<LIST>){
    my $label = $_;
    chop $label;
    push @labels,$label;
}
#print "LABELS: @labels\n";
my $whichLabel = $ENV{SGE_TASK_ID} - 1;
my $DataLabel = $labels[$whichLabel];

#------------------------------------------------------------------------
my ($data_dir, $rescue_dir, $hpss_home) = SetupOutput($Mode);
#------------------------------------------------------------------------
# check values of arguments
CheckArgs($SetupFile, $Mode, $TagDir, $DaqDir);
#------------------------------------------------------------------------
# deduce the file names
my ($tag_file,$daq_file,$vertex_input_file) = GetFiles($TagDir, $DaqDir, $DataLabel);
#------------------------------------------------------------------------
# extract star version, runID, etc...
my ($STAR_VERSION, $PROD_VERSION, $RunFseq) = GetInfo($TagDir, $DataLabel);
#------------------------------------------------------------------------
# user check

print "-" x 80, "\n";
print "Arguments and derived files and directories:\n",
  "SetupFile: $SetupFile\n",
  "Mode: $Mode\n",
  "TagDir: $TagDir\n",
  "DaqDir: $DaqDir\n",
  "DataDir: $data_dir\n",
  "DataLabel: $DataLabel\n",
  "NEvents: $NEvents\n",
  "tag file: $TagDir/$tag_file\n",
  "daq_file: $daq_file\n".
  "RunFseq: $RunFseq\n",
  "STAR_VERSION: $STAR_VERSION\n";

$Mode eq 'HPSS' and print "HPSS home: $hpss_home\nRescueDir: $rescue_dir\n";

#--------------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# following code is specific to application
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#--------------------------------------------------------------------------------

# read setup file

my @param_names = ("mult_fraction", "pid", "pt_low", "pt_high", "y_low", "y_high", "temperature", "zvertex_low", "zvertex_high", "geometry", "verbose", "GSTAR_mode", "ACC_mode");

my %SetupParams = ReadSetupFile($SetupFile, @param_names);

#----------------------------------------------------------------------------------
# define analysis chain

# chain name is extracted from filename of setup file
my ($chain_name, $path, $suffix) = fileparse( $SetupFile, '\.setup');

my $chain = new Chain_object($chain_name, $Mode, $data_dir, 
			     $STAR_VERSION, $PROD_VERSION, $RunFseq,
			     $rescue_dir);
$chain->SetupFile($SetupFile);

# remove these if you want to use the starofl default
#$chain->HpssHome($hpss_home);

#---------------------------------------------------------
# find vertices

my $zvertex_low = $SetupParams{zvertex_low};
my $zvertex_high = $SetupParams{zvertex_high};

my $root = new Process_object("TAGVERTICES",
			      $NEvents, 
			      $TagDir, 
			      $tag_file,
                              $zvertex_low, $zvertex_high,
                              $vertex_input_file);
$chain->Add($root);
#---------------------------------------------------------
# define gstar step
my $mult_fraction = $SetupParams{mult_fraction};
my $pid = $SetupParams{pid};
my $pt_low = $SetupParams{pt_low};
my $pt_high = $SetupParams{pt_high}; 
my $y_low = $SetupParams{y_low}; 
my $y_high = $SetupParams{y_high};
my $phi_low = $SetupParams{phi_low};
my $phi_high = $SetupParams{phi_high};
my $temp = $SetupParams{temperature}; 
my $geometry = $SetupParams{geometry};
my $verbose = $SetupParams{verbose}; 
my $mode = $SetupParams{GSTAR_mode};
my $acc_mode = $SetupParams{ACC_mode};
my $gstar;
# pmj 18/7/00: random seeds - ensures independent gstar runs
srand;
my $ran1 = int(rand 1000000) + 1;
my $ran2 = int(rand 1000000) + 1;

  #Others 
  my $y_sigma = 0.0; # forces a flat rapidity distribution
  my $seed = int(rand 1000000) + 1;
  my $system = $ENV{STAR_SYS};	   
  my $gentx_dir = "$code_dir/../GENTX/$system";
  my $gentx_ftpc_dir = "$code_dir/../GENTX_ftpc/$system";
  my $flatp_dir = "$code_dir/../FLATP/$system";

my $kumac_name = "/home/starofl/embedding/GSTAR/";
my @kumac_args;

#
# 02/07/03 - BUM - b field setting from daq file
#
my $field = GetBField($daq_file);

print "mode = $mode acc = $acc_mode field = $field\n";

# Choose kumac from field and mode...
if($mode eq "spectra"){
     @kumac_args = ($NEvents, $mult_fraction, $pid, $pt_low, $pt_high, 
	 $y_low, $y_high, $geometry, $verbose, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "phasespace_P06ib_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "phasespace_P06ib_revfullfield";
     }elsif($field eq "reversedhalf"){
         $kumac_name .= "run_phasespace_vertices_revhalffield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);

}elsif($mode eq "ftpc"){

     @kumac_args = ($NEvents, $mult_fraction, $pid, $pt_low, $pt_high, 
	 $y_low, $y_high,$geometry, $verbose, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_phasespace_ftpc_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_phasespace_ftpc_revfullfield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);

}elsif($mode eq "pi0"){

     @kumac_args = ($NEvents, $mult_fraction, $pid, $pt_low, $pt_high, 
	 $y_low, $y_high,$geometry, $verbose, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_phasespace_pi0_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_phasespace_pi0_revfullfield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);

}elsif($mode eq "tof"){

     @kumac_args = ($NEvents, $mult_fraction, $pid, $pt_low, $pt_high, 
	 $y_low, $y_high,$phi_low,$phi_high,$geometry, $verbose, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_phasespace_tof_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_phasespace_tof_revfullfield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);

}elsif($mode eq "strange"){

     my $gentx = new Process_object("GENTX", $gentx_dir, $pid, $NEvents, $mult_fraction, $y_low, 
                                    $y_high, $y_sigma, $pt_low, $pt_high, $temp, $seed);
     $chain->Add($gentx);
     @kumac_args = ($NEvents, $geometry, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_tx_strange_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_tx_strange_revfullfield";
     }elsif($field eq "reversedhalf"){
         $kumac_name .= "run_tx_strange_revhalffield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);
	 
}elsif($mode eq "flatp"){

     my $flatp = new Process_object("FLATP", $flatp_dir, $pid, $NEvents, $mult_fraction, $y_low, 
                                    $y_high, $y_sigma, $pt_low, $pt_high, $temp, $seed);
     $chain->Add($flatp);
     @kumac_args = ($NEvents, $geometry, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_flatp_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_flatp_revfullfield";
     }elsif($field eq "reversedhalf"){
         $kumac_name .= "run_flatp_revhalffield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);

}elsif($mode eq "ftpc_strange"){

     my $gentx = new Process_object("GENTX", $gentx_ftpc_dir, $pid, $NEvents, $mult_fraction, $y_low, 
                                    $y_high, $y_sigma, $pt_low, $pt_high, $temp, $seed);
     $chain->Add($gentx);
     @kumac_args = ($NEvents, $geometry, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_tx_ftpc_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_tx_ftpc_revfullfield";
     }elsif($field eq "reversedhalf"){
         $kumac_name .= "run_tx_ftpc_revhalffield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);

}elsif($mode eq "rich"){

     @kumac_args = ($NEvents, $mult_fraction, $pid, $pt_low, $pt_high, 
	 $y_low, $y_high,$geometry, $verbose, $ran1, $ran2);
     if($field eq "full"){
         $kumac_name .= "run_rich_vertices_fullfield";
     }elsif($field eq "reversedfull"){
         $kumac_name .= "run_rich_vertices_revfullfield";
     }else{
	 die "\n Don't know the field for $daq_file\n";
     }
     $gstar = new Process_object("GSTAR", $kumac_name, @kumac_args);
}else{
     die "\n Invalid value for GSTAR_mode: $mode\n";
}

     $chain->Add($gstar);
print "Selected GSTAR file $kumac_name\n";

#---------------------------------------------------------
# define bfc mixer step

# need name of gstar output file, 
# so run Chain_object::SetupFilenames here - will be run again when
# full chain is defined
$chain->SetupFilenames();

my $gstar_output = $gstar->OutputFile();
my $vertex_output = $root->OutputFile();

#my @bfcMixerArgs = ("fz", $gstar_output, "daq", $daq_file, $vertex_output, $zvertex_low, $zvertex_high);

my @bfcMixerArgs = ($daq_file, $gstar_output, $vertex_output, $zvertex_low, $zvertex_high, $mode, $acc_mode);

# pmj 18/7/00: new argument to specify non-library directory for bfcMixer.C
# if arg = "lib", library version will be taken using current STAR_LEVEL
# if macro not found using absolute path, chain will stop
# 
# elh ~7/20/01 arg = "lib" now uses local version of bfcMixer
# (see Process_object) along with STAR library version
# other option uses local libraries.

#my $bfc_dir = "lib";

#
# use local version of libraries:
#
my $bfc_dir = $code_dir;

# acceptance filter stuff
my $acc_dir = $code_dir;

my $bfcMixer = new Process_object("BFCMIXER", $bfc_dir, 
				  $NEvents, @bfcMixerArgs, $acc_dir);
$chain->Add($bfcMixer);

#---------------------------------------------------------
# called after all elements of chain are defined: default is that output file of each link is 
# input file to next link

$chain->SetupFilenames();

# user can change optionally input and output filenames at this point, e.g.
#$gstar->OutputFile("new_output_filename");

#---------------------------------------------------------
# run the chain

$chain->Run();

#---------------------------------------------------------
# all done

print "...all done on " . scalar localtime() ."\n";


#===========================================================================
