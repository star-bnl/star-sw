#! /usr/bin/perl 
#
# general object to wrap various processes (evgens, GSTAR, ROOT)
#
# pmj 8/7/00
#
#=========================================================
package Process_object;
#=========================================================
use strict;

use Cwd;

use File::stat;

use File::Copy;
use File::Find;
use File::Basename;
use Data::Dumper;

use vars qw(%StarDb); # file scope globals.

#
# files needed to access tpc information from the StarDb database.
# Not needed at rcf, but for reasons unknown, as of 12/2000,
# needed at pdsf.
#
# bum 3/5/2001
#$StarDb{P00hi} = '/auto/star/users/starreco/P00hi/StarDb/Calibrations/tpc';
# LSB temporary modification to allow P00hm running, put real one in later
#$StarDb{P00hm} = '/auto/star/users/starreco/P00hi/StarDb/Calibrations/tpc';

#=========================================================
1.;
#=========================================================

sub new{
  my $classname = shift;
  my $self = {};
  bless ($self, $classname);

  # initialize
  $self->_init(@_);

  return $self;
}
#========================================================
sub _init{

  my $self = shift;

  my $process_name = shift;  
  my @args = @_;

  #-------------------------------------------------

  $self->ProcessName($process_name);
  $self->ProcessArgs(@args);
  #-------------------------------------------------
  # initialization step: set output filetypes, etc.
  $self->Init();

}
#========================================================
sub ProcessName{
  my $self = shift;
  @_ and $self->{ProcessName} = shift;
  return $self->{ProcessName};
}
#========================================================
sub ProcessArgs{
  my $self = shift;
  @_ and do{
    my @args = @_;
    @{ $self->{ProcessArgs} } = @args;
  };
  return @{ $self->{ProcessArgs} };
}
#========================================================
sub InputFile{
  my $self = shift;
  @_ and $self->{InputFile} = shift;
  return $self->{InputFile};
}
#========================================================
sub OutputFile{
  my $self = shift;
  @_ and $self->{OutputFile} = shift;
  return $self->{OutputFile};
}
#========================================================
sub LogFile{
  my $self = shift;
  @_ and $self->{LogFile} = shift;
  return $self->{LogFile};
}
#========================================================
sub CshScript{
  my $self = shift;
  @_ and $self->{CshScript} = shift;
  return $self->{CshScript};
}
#========================================================
sub OutputFileType{
  my $self = shift;
  @_ and $self->{OutputFileType} = shift;
  return $self->{OutputFileType};
}
#========================================================
sub Chain{
  my $self = shift;
  @_ and $self->{Chain} = shift;
  return $self->{Chain};
}
#========================================================
sub Init{
  my $self = shift;

  my $process = $self->ProcessName;
  my $status = eval "\$self->$process('init')";

  $@ and die "Process_object::Init: Error calling process $process: $@ \n";
}
#========================================================
sub Run{
  my $self = shift;

  my $process = $self->ProcessName;
  my $status = eval "\$self->$process('run')";

  $@ and die "Process_object::Run: Error calling process $process: $@ \n";
}
#========================================================
sub CshHeaderString{

  my $self = shift;

  my $star_version = $self->Chain->StarVersion;

  #
  # bum : star version problems.  just try .starver
  #
  my $header_string = "#! /usr/local/bin/tcsh\n".
    "source \$GROUP_DIR/.starver $star_version\n";

  return $header_string;
}
#========================================================
sub ExecuteCshScript{

  my $self = shift;
  my $script = shift;

  print "ExecuteCshScript: script = $script\n";

  #-------------------------------------------------------
  # run kumac, pipe both STDOUT and STDERR (see PERL Cookbook 16.7)
  open SCRIPTSESSION, "$script 2>&1 |"  or die "can't fork: $!";

  #-------------------------------------------------------
  # open logfile, dump session into it

  my $logfile = $self->LogFile();
  open LOGFILE, "> $logfile";

  while (my $line = <SCRIPTSESSION>){
    print LOGFILE $line;

    # also send log info to screen...
    print $line;

  }
  #-------------------------------------------------------
  close SCRIPTSESSION;
  close LOGFILE;

}
#========================================================
#
# **** Process-specific routines appear below here ****
#
#========================================================
sub GSTAR{

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("fz");
  }
  elsif ($switch eq 'run'){

    #print "In Process_object::GSTAR, here is dump of object: \n";
    #print Dumper(\$self), "\n";

    #-------------------------------------------------------

    my @process_args = $self->ProcessArgs();

    my $kumac_name = shift @process_args;
    my @commands = @process_args;
    
    my $command_string = join ' ', @commands;
          
    #------------------------------------------------
    # create temporary csh script

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };
    #------------------------------------------------
    my $input_filename = $self->InputFile();
    my $output_filename = $self->OutputFile();
    #------------------------------------------------

    my $temp_dir = $self->Chain->TempDir;

    my $header_string = $self->CshHeaderString();
    
    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    print SCRIPT "$header_string",
    "cd $temp_dir\n",
    "ln -s /home/starofl/embedding/GSTAR/convPi0Selection.g convPi0Selection.g\n",
    "ln -s /home/starofl/embedding/GSTAR/convPhotonSelection.g convPhotonSelection.g\n",
    "ln -s \$STAR/pams/sim/gphysdata/y.dat bertaf.dat\n",
    "ln -s \$STAR/pams/sim/gphysdata/chetc.dat chetc.dat\n",
    "ln -s \$STAR/pams/sim/gphysdata/flukaaf.dat flukaaf.dat\n",
    "ln -s \$STAR/pams/sim/gphysdata/xsneut.dat xsneut.dat\n",
    "ln -s $input_filename input_link\n",
    "starsim -w 0<<EOF\n",
    "exec $kumac_name input_link $output_filename $command_string\n",
    "quit\n",
    "EOF\n",
    "mv pi0.hbook ../\n";
    
    close SCRIPT;
    
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);
    
  }
  elsif ($switch eq 'end'){
  }

}
#========================================================
# get the vertices from tags db
#
sub TAGVERTICES{
  
  my $self = shift;
  my $switch = shift;

  if ($switch eq 'init'){
    $self->OutputFileType('dat');
  }
  elsif ($switch eq 'run'){
    my @args = $self->ProcessArgs();
    # input args are
    # 1. nevent, 2. tags dir, 3. tags.root file
    # 4. zvertex_low 5. zvertex_high
    my $nEvent = shift @args;
    my $inDir = shift @args;
    my $inFile = shift @args;
    my $zvertex_low = shift @args;
    my $zvertex_high = shift @args;

    #
    # name of the output file set by macro according to the input
    #
    my $vertexFile = shift @args; 

    #
    # mv the output file (vertices.dat) to this
    #
    my $outFile = $self->OutputFile();

    my $macro_dir = "/u/starofl/embedding";

    my $rootString 
      = qq{ .x $macro_dir/getVerticesFromTags_v4.C($nEvent,"$inDir","$inFile",$zvertex_low,$zvertex_high) };
    my $tempDir = $self->Chain->TempDir();

    # temp script
    my $script = $self->CshScript();
    my $header = $self->CshHeaderString();

    my $workDir = $self->Chain->TempDir;

    open(SCRIPT,">$script") or die "$script: $!\n";
    
    print SCRIPT <<FINIS;
$header
cd $workDir
echo 'root string: $rootString'
root4star -b<<EOF
$rootString
.q
EOF
echo 'Vertex from tags done, moving file...'
#mv $vertexFile $outFile
cp $vertexFile $outFile  #leave
FINIS
    close SCRIPT;
    chmod 0775, $script;

    $self->ExecuteCshScript($script);

  }
  elsif ($switch eq 'end'){}
}
     
#========================================================
sub VERTICES{

  # currently for getting vertices from StEvent - needs to be generalized

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("dat");
  }
  elsif ($switch eq 'run'){

    my @process_args = $self->ProcessArgs();

    my $work_dir = shift @process_args;
    my $input_dir = shift @process_args;
    my $input_file = shift @process_args;
    my $n_events = shift @process_args;
    my $vertex_input_file = shift @process_args;
    #------------------------------------------------
#    my $input_filename = $self->InputFile();

    my $output_filename = $self->OutputFile();

    # root doesn't like very long arguments to bfc - set aliases to input and output files
#    my $input_string = "char *infile = \"$input_filename\"";
    my $output_string = "char *outfile = \"$output_filename\"";

    # need to source group_env.csh, otherwise some shared libs not found (!?)
    my $root_string =   ".x doEvents.C($n_events, \"$input_dir\", \"$input_file\")";

    my $temp_dir = $self->Chain->TempDir;
    
    #------------------------------------------------
    # create temporary csh script

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };

     my $header_string = $self->CshHeaderString();   

    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    print SCRIPT "$header_string",
    "cd $work_dir\n",
    "echo \'root string: $root_string\'\n",
    "root4star -b<<EOF\n",
    "$root_string\n",
    ".q\n",
    "EOF\n",
    "mv $vertex_input_file $output_filename\n";
    
    close SCRIPT;
    
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);
    
  }
  elsif ($switch eq 'end'){
  }

}
#========================================================
sub BFC{

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("root");
  }
  elsif ($switch eq 'run'){
    my $data_dir = $self->Chain->DataDir;

    my @process_args = $self->ProcessArgs();

    my $bfc_dir = shift @process_args;
    my $n_events = shift @process_args;
    my $bfc_arguments = shift @process_args;
    #------------------------------------------------
    my $input_filename = $self->InputFile();
    my $output_filename = $self->OutputFile();

    # root doesn't like very long arguments to bfc - set aliases to input and output files
    my $input_string = "char *infile = \"$input_filename\"";
    my $output_string;
    if($output_filename){
      $output_string = "char *outfile = \"$output_filename\"";
    }
    else {
      $output_string = "char *outfile = 0";
    }

    # need to source group_env.csh, otherwise some shared libs not found (!?)
    my $bfc_string =   ".x bfc.C($n_events, \"$bfc_arguments\", infile, outfile)";
    
    #------------------------------------------------
    # create temporary csh script

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };

    my $header_string = $self->CshHeaderString();    

    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    print SCRIPT "$header_string",
    "echo \'input: $input_string\'\n",
    "echo \'output: $output_string\'\n",
    "echo \'bfc string: $bfc_string\'\n",
    "cd $data_dir\n",
    "root4star -b<<EOF\n",
    "$input_string\n",
    "$output_string\n",
    "$bfc_string\n",
    ".q\n",
    "EOF\n",
    "\\rm -rf fort.99\n";
    
    close SCRIPT;
    
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);
    
  }
  elsif ($switch eq 'end'){
  }

}
#========================================================
sub BFCMIXER{

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("root");
  }
  elsif ($switch eq 'run'){
    
    my ($bfc_dir, $n_events, $fzfile, $daqfile, $vertexfile,$zvertex_low, $zvertex_high, $mode, $acc_mode, $acc_dir) = $self->ProcessArgs();
    
    #------------------------------------------------
    my $temp_dir = $self->Chain->TempDir;
    my $data_dir = $self->Chain->DataDir;

    my $macro;
    if ($bfc_dir eq "lib"){
      # $macro = "bfcMixer.C";  # library version
      $macro = "$bfc_dir/bfcMixer_v4_noFTPC.C";  # use local version elh
      -e $macro or die "Error in BFCMIXER: macro $macro not found\n";
    }
    else{
      $macro = "$bfc_dir/bfcMixer_v4_noFTPC.C";
      -e $macro or die "Error in BFCMIXER: macro $macro not found\n";
    }

    # Make links to ACCFILTER directory for modified bfChain and V0AccMaker
    # LSB 12/4/00

    # pmj 15/12/00 generalize so that this is also usable for other applications
    
    if(defined $acc_dir){

      print "Specified directory for acceptance filter is: $acc_dir \n";
      if ($acc_dir eq "lib" ){
	print "No acceptance filter being used\n";
      }
      else{
	my $target = "$acc_dir/.share";
	my $link = "$data_dir/.share";
	if (!-l $link){ 
	  print "Making the link $link -> $target\n";
	  my $ok = symlink $target, $link;
	  $ok or die "Could not symlink $target $link\n";
	}

	#--- pmj 15/2/01: generalize to run on sun
	#my $sys_string = ".i386_linux24";
	my $sys_string = ".rh80_gcc32";
	$ENV{BFARCH} eq "SunOS5_CC5" and $sys_string = ".sun4x_56_CC5";

 	#my $target = "$acc_dir/.i386_linux24";
	#my $link = "$data_dir/.i386_linux24";

	my $target = "$acc_dir/$sys_string";
	my $link = "$data_dir/$sys_string";
	#---

	if (!-l $link){ 
	  print "Making the link $link -> $target\n";
	  my $ok = symlink $target, $link;
	  $ok or die "Could not symlink $target $link\n";
	}
      } #end acc modifications

    }
    else{

      # bum 3/6/01
      # if we're not using the default star library version of trsmaker
      # (or some other maker), then make a soft link in the data_dir
      # back to the bfc_dir (where the bfcMixer code resides) 
      # (like the accfilter stuff)
      # this is basically a copy of the above and can be combined with
      # the above code if anyone is able and willing.
      
      unless( $bfc_dir eq 'lib'){
	my $target = "$bfc_dir/.share";
	my $link = "$data_dir/.share";
	if (!-l $link){ 
	  print "Making the link $link -> $target\n";
	  my $ok = symlink $target, $link;
	  $ok or die "Could not symlink $target $link\n";
	}
	my $sys_string = ".i386_linux24";
	$ENV{BFARCH} eq "SunOS5_CC5" and $sys_string = ".sun4x_56_CC5";

	my $target = "$bfc_dir/$sys_string";
	my $link = "$data_dir/$sys_string";
	#---

	if (!-l $link){ 
	  print "Making the link $link -> $target\n";
	  my $ok = symlink $target, $link;
	  $ok or die "Could not symlink $target $link\n";
	}

      } 
    }      

    # pmj 1/12/00

    # root doesn't like very long arguments to bfc - set aliases to files
    my $fz_string = "char *fzfile = \"$fzfile\"";
    my $daq_string = "char *daqfile = \"$daqfile\"";
    my $vertex_string = "char *vertexfile = \"$vertexfile\"";
    my $flag_string = "char *mode = \"$mode\"";
    my $accflag_string = "char *acc_mode = \"$acc_mode\"";
#    my $bfc_string = ".x $macro($n_events, \"$kind1\", fzfile, \"$kind2\", daqfile, vertexfile, $zvertex_low, $zvertex_high)";
    my $bfc_string = ".x $macro($n_events, fzfile, daqfile, vertexfile, $zvertex_low, $zvertex_high, mode, acc_mode)";

    #---  pmj 1/12/00 end of change

    #------------------------------------------------
    # create temporary csh script

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };

    my $header_string = $self->CshHeaderString();
    
    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    my $move_files = 0;

    print "bfc_string:\n",$bfc_string,"\n";

    print SCRIPT <<FINIS;
$header_string
cd $data_dir
root4star -b<<EOF
$fz_string
$daq_string
$vertex_string
$flag_string
$accflag_string
$bfc_string
.q
EOF
FINIS
    
    close SCRIPT;
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);
    #------------------------------------------------
    # move root files to data directory

# pmj 19/7/00 this should go away if moving files not needed...

    $move_files and do{
    
      # get filename with path
      
      my $filetype = $self->OutputFileType();
      my $output_filename = $self->OutputFile();
      
      (my $output_string = $output_filename) =~ s/$filetype//;
      
      opendir DIR, $temp_dir;
      
      my $file;
      
      while ( defined ($file = readdir(DIR) ) ){
	
	$file =~ /root$/ or next;
	
	(my $filetype = $file) =~ s/output\.//;
	my $newfile = $output_string.$filetype;
	
	my $full_file = "$temp_dir/$file";
	
	my $status = system("mv $full_file $newfile");
      }
    };
  }
  elsif ($switch eq 'end'){
  }
  
}
#========================================================
sub HIJING{

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("nt");
  }
  elsif ($switch eq 'run'){

    my @process_args = $self->ProcessArgs();

    my $setup_file = shift @process_args;
    #------------------------------------------------
    my $input_filename = $self->InputFile();
    my $output_filename = $self->OutputFile();
    #------------------------------------------------
    # create temporary csh script

    # current library version of HIJING (15/7/00) has some peculiar behaviour:
    # - input params only via file named hijev.inp
    # - output is named evgen.N.nt, where N is some arbitrary integer assigned by the code
    # - output is put automatically in directory where process is run

    my $executable = "/afs/rhic/star/users/jacobs/evgen/pams/gen/hijing/hijjet.x";
    my $temp_dir = $self->Chain->TempDir();

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };

     my $header_string = $self->CshHeaderString();
   
    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    print SCRIPT "$header_string",
    "cd $temp_dir\n",
    "ln -s $setup_file hijev.inp\n",
    "$executable\n",
    "cp `ls evgen*` $output_filename\n";
    "EOF\n";

    close SCRIPT;
    
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);
    
  }
  elsif ($switch eq 'end'){
  }

}
#========================================================
# Additional object to run GENTX - simple particle distribution generator
# Lee Barnby 29/11/00

sub GENTX{

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("tx");
  }
  elsif ($switch eq 'run'){

    my @process_args = $self->ProcessArgs();

    # pmj 16/12/00
    my $gentx_dir = shift @process_args;

    #Other args e.g my $name = shift @process_args
    my $pid  = shift @process_args;
    my $nev  = shift @process_args;
    my $ppe  = shift @process_args;
    my $y_lo = shift @process_args;
    my $y_hi = shift @process_args;
    my $y_mean  = 0.0;   #Would we ever want anything else?
    my $y_sigma = shift @process_args;
    my $pt_lo = shift @process_args;
    my $pt_hi = shift @process_args;
    my $temp  = shift @process_args;
    my $seed  = shift @process_args;

    #------------------------------------------------
    # create temporary csh script

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };
    #------------------------------------------------
    my $input_filename = $self->InputFile();
    my $output_filename = $self->OutputFile();
    #------------------------------------------------

    my $header_string = $self->CshHeaderString();
   
    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    print SCRIPT "$header_string",
    "$gentx_dir/gentx <<EOF\n",
    "$input_filename\n",  #vertices file
    "$output_filename\n", #output file
    "$pid\n",             #GEANT ID of particle 
    "$nev\n",             #number of events
    "$ppe\n",             #number of particles per event actually generated = 
                          # ppe * multiplicity (from vertex file) * 0.01
    "$y_lo\n",            #rapidity lower bound
    "$y_hi\n",            #rapidity upper bound 
    "$y_mean\n",          #mean of rapidity distribution
    "$y_sigma\n",         #width of distribution OR =0 for flat
    "$pt_lo\n",           #pt lower limit
    "$pt_hi\n",           #pt upper limit
    "$temp\n",            #inverse slope in MeV !!
    "$seed\n",            #random seed
    "EOF\n";
    
    close SCRIPT;
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);

  }
  elsif ($switch eq 'end'){
  }
}
#========================================================
# Additional object to run FLATP - generate particles
# flat in p (instead of pt)
# adapted from GENTX 12/1/04 elh

sub FLATP{

  my $self = shift;
  my $switch = shift;
  #-------------------------------------------------------

  if ($switch eq 'init'){
    $self->OutputFileType("tx");
  }
  elsif ($switch eq 'run'){

    my @process_args = $self->ProcessArgs();

    # pmj 16/12/00
    my $flatp_dir = shift @process_args;

    #Other args e.g my $name = shift @process_args
    my $pid  = shift @process_args;
    my $nev  = shift @process_args;
    my $ppe  = shift @process_args;
    my $y_lo = shift @process_args;
    my $y_hi = shift @process_args;
    my $y_mean  = 0.0;
    my $y_sigma = shift @process_args;
    my $pt_lo = shift @process_args;
    my $pt_hi = shift @process_args;
    my $temp  = shift @process_args;
    my $seed  = shift @process_args;

    #------------------------------------------------
    # create temporary csh script

    my $script = $self->CshScript();
    
    # make sure it disappears at the end...
    #END { unlink($script) };
    #------------------------------------------------
    my $input_filename = $self->InputFile();
    my $output_filename = $self->OutputFile();
    #------------------------------------------------

    my $header_string = $self->CshHeaderString();
   
    open (SCRIPT, "> $script") or die "Cannot open $script: $!\n";

    print SCRIPT "$header_string",
    "$flatp_dir/flatp <<EOF\n",
    "$input_filename\n",  #vertices file
    "$output_filename\n", #output file
    "$pid\n",             #GEANT ID of particle 
    "$nev\n",             #number of events
    "$ppe\n",             #number of particles per event actually generated = 
                          # ppe * multiplicity (from vertex file) * 0.01
    "$y_lo\n",            #rapidity lower bound
    "$y_hi\n",            #rapidity upper bound 
    "$y_mean\n",          #mean of rapidity distribution
    "$y_sigma\n",         #width of distribution OR =0 for flat
    "$pt_lo\n",           #p lower limit
    "$pt_hi\n",           #p upper limit
    "$temp\n",            #inverse slope in MeV !!
    "$seed\n",            #random seed
    "EOF\n";
    
    close SCRIPT;
    chmod 0755, $script;
    #------------------------------------------------
    $self->ExecuteCshScript($script);

  }
  elsif ($switch eq 'end'){
  }
}

