#! /usr/bin/perl

use Process_object;
use Chain_object;
use File::Basename;
use Cwd;

use strict;

#===========================================================================
1.;
#===========================================================================

sub GetArgs{

  # Input arguments...
  
  # file to set up chain name, GSTAR parameters, etc.
  my $SetupFile = $ARGV[0]; 
  
  # values are: HPSS (data written to /scratch and then staged to HPSS)
  # or DISK (data written to $disk/... see below)
  my $Mode = $ARGV[1]; 
  
  # dir of daq files
  my $DaqDir   = $ARGV[2]; 
  -l $DaqDir and $DaqDir = readlink $DaqDir;
  $DaqDir = cwd() . "/$DaqDir" if $DaqDir !~ m{^/};
  
  # dir of TagDB files
  my $TagDir   = $ARGV[3]; 
  -l $TagDir and $TagDir = readlink $TagDir;
  
  # e.g. st_physics_1243017_raw_0001
  #my $DataLabel      = $ARGV[4]; 
  
  # disk Vault
  my $DiskVault = $ARGV[4];

  # daq Label
  my $DaqLabel = $ARGV[5];

  # number of events
  my $NEvents   = $ARGV[6] || 999999; 
  
  if(!($SetupFile && $Mode && $DaqDir)){
    print "Error: insufficent arguments\n";
    print "\nUsage:\n",
    "$0 SetupFile Mode TagDir DaqDir DiskVault DaqLabel [Nevents]\n\n";
    exit 1;
  }
  
  return ($SetupFile, $Mode, $DaqDir, $TagDir, $DiskVault, $DaqLabel, $NEvents);

}

#===========================================================================
sub SetupOutput{

  my $Mode = shift;

  # the output of the embedding jobs!
  # if mode is DISK, the output is written to $disk;
  # if mode is HPSS, the output is first written to $scratch, 
  # then shipped to hpss. 
  # if something goes wrong with hpss, the files are moved to $rescue_dir
  # 02/06/02
  # - BUM if mode is BOTH, writes to HPSS and to disk ($disk)

  my $data_dir;

  my $this_dir = cwd();
  my $disk = "$this_dir/data";
  my $userName = $ENV{'LOGNAME'};
  my $scratch = "/scratch/".$userName;

#  my $rescue_dir = "/auto/pdsfdv09/star/starofl/save";
  my $rescue_dir = "/auto/pdsfdv35/rhstar/starofl/save";
#  my $rescue_dir = "/beta/starprod/save";
  my $hpss_home = "/nersc/projects/starofl"; 
 
  if ($Mode eq "HPSS"){
    $data_dir = $scratch;
  }
  elsif ($Mode eq "DISK"){
    $data_dir = $disk;
  }
  elsif($Mode eq "BOTH"){
    $data_dir = $disk;
  }
  else{
    die "Mode $Mode not defined\n";
  }

  return ($data_dir, $rescue_dir, $hpss_home);

}  
#===========================================================================

sub CheckArgs{

  my $SetupFile = shift;
  my $Mode = shift;
  my $TagDir = shift;
  my $DaqDir = shift;

  -e $SetupFile or die "SetupFile $SetupFile not found \n";
  
  # 02/06/03 BUM - already checked in SetupOutput()
  #$Mode eq "HPSS" or $Mode eq "DISK" or die "Mode $Mode not recognized \n";
  
  -d $TagDir or die "TagDir: $TagDir not a directory\n";
  -d $DaqDir or die "DaqDir: $DaqDir not a directory\n";
}
  
#===========================================================================

# deduce the names of the dst and vertex file

sub GetFiles{  

  my $TagDir = shift;
  my $DaqDir = shift;
  my $DataLabel = shift;

  my $tag_file = "$DataLabel.tags.root";
  my $daq_file = "$DaqDir/$DataLabel.daq";
  my $vertex_input_file = "$DataLabel.vertices.dat";
  
  # if the daq file is a link, resolve the link
  -l $daq_file and $daq_file = readlink $daq_file;

  -e "$TagDir/$tag_file" or die "$TagDir/$tag_file: $!\n";
  -e "$daq_file" or die "$daq_file: $!\n";

  return ($tag_file, $daq_file, $vertex_input_file);
  
}
#===========================================================================
sub GetInfo{

  my $TagDir = shift;
  my $DataLabel = shift;

  # extract STAR library version from $TagDir
  # bum: e.g. production version : P00hi
  #           star version       : SL00i
  
  $TagDir =~ m{/(?:reco|tags)/P(\d{2})\w{1}(\w{1})};
  my $STAR_VERSION = "SL$1$2";
  
  $TagDir =~ m{/(?:reco|tags)/(P\w+)/};
  my $PROD_VERSION = $1;
  
  # extract run and fseq from $DataLabel
  
  (my $RunFseq = $DataLabel) =~ s/st_physics_adc_//;
  # leave in one underscore to seperate run and fseq
  $RunFseq =~ s/_raw//;

  return ($STAR_VERSION, $PROD_VERSION, $RunFseq);
}
#===========================================================================

sub ReadSetupFile{

  my $SetupFile = shift;
  my @param_names = @_;

  open SETUP, $SetupFile or die "Cannot open SetupFile $SetupFile: $!\n";

  my %SetupParams;
  
  my $line;
  while ( defined ($line = <SETUP>) ){
    $line =~ /(\w+)=(\S+)/ or next;
    $SetupParams{$1} = $2;
  }
  
  print "-" x 80, "\n";
  print "Contents of Setup file:\n";
  
  my $errors = 0;
  
  for my $key (@param_names) {
    if ( defined $SetupParams{$key} ){
      print "$key = ",$SetupParams{$key},"\n";
    }
    else{
      print "Error in SetupParams: key=$key not defined\n";
      $errors++;
    }
  }
  
  print "-" x 80, "\n";
  
  $errors and die "Errors in setup file, goodbye\n";

  return %SetupParams;
}
#===========================================================================
# 02/07/03 - BUM - return B field setting from run number in daq file

sub GetBField{
  my $daq_file = shift;
  my $field = undef;

#minbias full field:
my @mbffruns = (2270008,2271003,2272084,2272085,2272086,2273003,2273004,2273005,2274003,2275002,4026009,4034028,4035018,4036002,4036043,5028118,5031046,5032001,5033119,6050022);
foreach my $run (@mbffruns){$daq_file =~ /$run/ and $field = "full";}

#central full field:
my @cffruns = (2277009,2286017,23120072313002,2318004,2318005,2318006,2319004,2320004,2320008,2321003,2322001,2322002,2322004,2323001,2323002);
foreach my $run (@cffruns){$daq_file =~ /$run/ and $field = "full";}

#minbias reversed field:
my @mbrfruns = (2254002,2256003,2256004,2256007,2256009,2257001,2257003,2257004,2269001,2298001,2301002,2303001,2304004,2304005,2310043,2310044,2326020,4044004,4049021,4059014,5015053,5027004,5027044,5087079,5088065,5089071,5090009,5091003);
foreach my $run (@mbrfruns){$daq_file =~ /$run/ and $field = "reversedfull";}

#central reversed field:
my @crfruns = (2303005,2326004,2327005);
foreach my $run (@crfruns){$daq_file =~ /$run/ and $field = "reversedfull";}

#central reversed half field (or 20GeV):
my @crhfruns = (2308001,2329088,2329092);
foreach my $run (@crhfruns){$daq_file =~ /$run/ and $field = "reversedhalf";}

#MinbiasVertex reversed half field:
my @mbrhfruns = (2235004,2244018,2245002,2305006,2306004,2307006,2308006);
foreach my $run (@mbrhfruns){$daq_file =~ /$run/ and $field = "reversedhalf";}

#productionHalfLow half field:
my @phlhfruns = (5060047,5061085,5062027);
foreach my $run (@phlhfruns){$daq_file =~ /$run/ and $field = "half";}

#productionHigh full field:
my @phffruns = (5043073,5048024);
foreach my $run (@phffruns){$daq_file =~ /$run/ and $field = "full";}

#productionMid full field:
my @pmffruns = (5046079,5053115,5054115,5052114);
foreach my $run (@pmffruns){$daq_file =~ /$run/ and $field = "full";}

#productionHigh reversed full field:
my @phffruns = (5081032,5081053);
foreach my $run (@phffruns){$daq_file =~ /$run/ and $field = "full";}

#pp Full Field
my @ppPFF = (612076,6126080,6126080,6127037,6127039,6134003,6134025,6148057);
foreach my $run (@ppPFF){$daq_file =~ /$run/ and $field = "full";}

  #return $fieldValue{$field};

  my @cuFF = (6050018,6052070,6052072);
  foreach my $run (@cuFF) { $daq_file =~ /$run/ and $field = "full";}

  my @cuRFF = (6022054, 6038085, 6038088, 6044081, 6045070, 6044086, 6044089);
  foreach my $run (@cuRFF) { $daq_file =~ /$run/ and $field = "reversedfull";}


  my $dirField;
  if(!$field ){
      $dirField = `dirname $daq_file`;
      $field = `basename $dirField`;
  }

  if ($field eq "FullField" )         { $field="full"; }
  if ($field eq "ReversedFullField" ) { $field="reversedfull"; }

  my $magscale = 0;
  $daq_file =~ /(\w+)(\d+)(\w+)(\d+)/;
  my $runnumber = $1;
  print "Runnumber = $runnumber\n";

  return $field;

###  could consider a method like this
#  my $magscale = `get_file_list.pl -keys 'magscale' -distinct -cond 'runnumber=$runnumber'`;
#  print "Magscale = $magscale\n";
#  return $magscale;
}


