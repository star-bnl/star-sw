#! /usr/bin/perl 
#
# general object to define processing chain
#
# pmj 8/7/00
#
#=========================================================
package Chain_object;
#=========================================================
use strict;

use Cwd;

use File::stat;
use File::Path;

use File::Copy;
use File::Find;
use File::Basename;

use Net::FTP;
use Net::Netrc;

use Data::Dumper;
use Cwd;

# globals in order to use them in the END subroutine
use vars qw($MODE $DATADIR);

#=========================================================
1.;

#
# default variables
#
my $HPSSHOME = "/nersc/projects/starofl";
my $HPSSHOST = "archive.nersc.gov";
my $SAVEDIR  = "/auto/pdsfdv35/rhstar/starprod/save";

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

  my $chain_name = shift;  
  @_ and my $Mode = shift;
  @_ and my $top_dir = shift;
  @_ and my $STAR_VERSION = shift;
  @_ and my $PROD_VERSION = shift;
  @_ and my $RunFseq = shift;
  @_ and my $saveDir = shift;
  #-------------------------------------------------

  $self->ChainName($chain_name);

  my $procID = $$;
  $self->ChainProcID($procID);

  my $IDstring = "$RunFseq.$procID";
  $self->IDstring($IDstring);

  # running mode: DISK or HPSS
  $self->Mode($Mode);

  # directory for data
  defined $top_dir or $top_dir = ".";
# pmj 19/7/00
  my $data_dir = "$top_dir/$PROD_VERSION/$chain_name/$IDstring";

  $self->DataDir($data_dir);

  #directory for temporary files
  my $temp_dir = "$data_dir/temp.$IDstring";
  $self->TempDir($temp_dir);

  # log file for chain
  my $logfile = "$data_dir/$chain_name.$IDstring.log";
  $self->ChainLogFile($logfile);

  # STAR version
  $self->StarVersion($STAR_VERSION);

  # production version
  $self->ProdVersion($PROD_VERSION);

  # Run and Fseq
  $self->RunFseq($RunFseq);

  # hpss stuff.  initialize with the default values.
  # the user can change it later
  $self->HpssHome($HPSSHOME);
  $self->HpssHost($HPSSHOST);

  # the directory to move the files if something 
  # gets screwed up when shipping to hpss

  $Mode eq "HPSS" and do{
    $saveDir = $SAVEDIR if !defined $saveDir;
    
    $self->SaveDir("$saveDir/$PROD_VERSION/$chain_name");
  };
  
  #
  # initialize globals to use in END
  #
  $MODE    = $self->Mode();
  $DATADIR = $self->DataDir();

}
#========================================================
sub ChainName{
  my $self = shift;
  @_ and $self->{ChainName} = shift;
  return $self->{ChainName};
}
#========================================================
sub ChainProcID{
  my $self = shift;
  @_ and $self->{ChainProcID} = shift;
  return $self->{ChainProcID};
}
#========================================================
sub IDstring{
  my $self = shift;
  @_ and $self->{IDstring} = shift;
  return $self->{IDstring};
}
#========================================================
sub Mode{
  my $self = shift;
  @_ and $self->{Mode} = shift;
  return $self->{Mode};
}
#========================================================
sub StarVersion{
  my $self = shift;
  @_ and $self->{StarVersion} = shift;
  return $self->{StarVersion};
}
#========================================================
sub ProdVersion{
  my $self = shift;
  @_ and $self->{ProdVersion} = shift;
  return $self->{ProdVersion};
}
#========================================================
sub RunFseq{
  my $self = shift;
  @_ and $self->{RunFseq} = shift;
  return $self->{RunFseq};
}
#========================================================
sub SetupFile{
  my $self = shift;
  @_ and $self->{SetupFile} = shift;
  return $self->{SetupFile};
}
#========================================================
sub ChainLogFile{
  my $self = shift;
  @_ and $self->{ChainLogFile} = shift;
  return $self->{ChainLogFile};
}
#========================================================
sub HpssHome{
  my $self = shift;
  @_ and $self->{HpssHome} = shift;
  return $self->{HpssHome};
}
#========================================================
sub HpssHost{
  my $self = shift;
  @_ and $self->{HpssHost} = shift;
  return $self->{HpssHost};
}
#========================================================
sub SaveDir{
  my $self = shift;
  return $self->{SaveDir} if !@_;

  # else
  my $dir = shift;
  
  if (-e $dir){
    -d $dir 
      or die "Chain_object::SaveDir error: $dir exists but is not a directory\n";
  }
  else{
    mkpath $dir;
    -d $dir 
      or die "Chain_object::SaveDir error: cannot create $dir\n";
  }
  $self->{SaveDir} = $dir;
}
#========================================================
sub DataDir{
  my $self = shift;
  @_ and do{
    my $dir = shift;

    # directory to write data
    $self->{DataDir} = $dir;
    if (-e $dir){
      -d $dir or die "Chain_object::DataDir error: $dir exists but is not a directory\n";
    }
    else{
	system( "id");
#      system( "mkdir $dir");
	mkpath $dir;
	print "ELH dir = $dir\n";
#      mkpath $dir;
      -d $dir or die "Chain_object::DataDir error: cannot create $dir\n";
    }
  };
return $self->{DataDir};
}
#========================================================
sub TempDir{
  my $self = shift;
  @_ and do{
    my $dir = shift;

    # directory for temporary files
    $self->{TempDir} = $dir;
    if (-e $dir){
      die "Chain_object::TempDir error: $dir already exists\n";
    }
    else{
      system( "mkdir $dir");
      -d $dir or die "Chain_object::TempDir error: cannot create $dir\n";
    }
  };
return $self->{TempDir};
}
#========================================================
sub Add{
  my $self = shift;

  # adds a process to chain
  @_ and do{
    my $process = shift;
    push @{$self->{Processes}}, $process;

    # give process a pointer to this chain object
    $process->Chain($self);

    my $process_name = $process->ProcessName();

    # log file for running this process
    my $temp_dir = $self->TempDir();
    my $logfile = "$temp_dir/$process_name.log";
    $process->LogFile($logfile);

    # csh script for running this process
    my $script = "$temp_dir/$process_name.csh";
    $process->CshScript($script);

  };

  return @{ $self->{Processes} };
}
#========================================================
sub SetupFilenames{
  my $self = shift;

  # sets up filenames of successive processes in chain - output of one is input to the next
  my $icount = 0;
  my @processes =  @{ $self->{Processes} };

  my $top_dir =  $self->DataDir();

  # use process ID of this chain to uniquely  label outputs
  my $chain_procID = $self->ChainProcID();
  my $chain_name = $self->ChainName();
  my $RunFseq = $self->RunFseq();

  my ($process,  $process_name, $old_process);

  for (my $icount = 0; $icount <= $#processes; $icount++){
    
    $process = $processes[$icount];
    $process_name = $process->ProcessName();

    # first process has no input
    if ($icount == 0 ){
        $process->InputFile("none");
      }
    else{
      my $infile = $old_process->OutputFile();
      $process->InputFile($infile);
    }
    
    my $file_type = $process->OutputFileType();
    my $full_output_name = "$top_dir/$process_name.$chain_name".
      ".$RunFseq.$chain_procID.$file_type";
    $process->OutputFile($full_output_name);
    $old_process = $process;
  }
}
#========================================================
sub Run{
  my $self = shift;

  #---------------------------------------------------------

  my @process_list = @{ $self->{Processes} };
  my $chain_name = $self->ChainName();

  #---------------------------------------------------------
  # open chain logfile
  
  my $now = localtime();

  my $chain_log = $self->ChainLogFile();
  my $prod_version = $self->ProdVersion();
  my $star_version = $self->StarVersion();

  my $IDstring = $self->IDstring();
  my $Mode = $self->Mode();
  my $RunFseq = $self->RunFseq();

  print "Opening chain logfile $chain_log\n";

  open CHAINLOG, ">$chain_log";

  # header
  print CHAINLOG '=' x 80;

  print CHAINLOG "\nChain $chain_name\nTime is $now\n",
  "STAR_VERSION = $star_version\n";

  $RunFseq and print CHAINLOG "Run and Fseq = $RunFseq\n";

  print CHAINLOG "IO Mode: $Mode\n";

  my $host = $ENV{HOST};
  print CHAINLOG "Host: $host\n";

  print CHAINLOG '=' x 80, "\n\n";

  #---

  print CHAINLOG '=' x 80, "\nProcess list:\n";
  foreach my $process (  @process_list ){
    print CHAINLOG $process->ProcessName(),"\n";
  }
  print CHAINLOG '=' x 80, "\n\n";
  #---------------------------------------------------------
  # runs sequence of processes comprising chain
  
  foreach my $process (  @process_list ){

    print CHAINLOG '%' x 80, "\n Here is logfile for process ", $process->ProcessName(),"\n",'%' x 80, "\n\n";

    $process->Run();

    my $proc_log = $process->LogFile();
    open PROCLOG, $proc_log;
    while (my $line = <PROCLOG>){
      print CHAINLOG $line;
    }
    close PROCLOG;

  }
  #---------------------------------------------------------
  # if Mode = HPSS, write files to HPSS and clean up /scratch
  # 02/06/02
  # - BUM if Mode = BOTH, write files to HPSS but dont clean up disk

  my $hpss_home     = $self->HpssHome();
  my $hpss_top_dir  = "$hpss_home/embedding/$prod_version";
  my $hpss_data_dir = "$hpss_top_dir/$chain_name/$IDstring";
  my $host          = $self->HpssHost();
 
  my ($ftp,$status); 

  if($Mode eq 'HPSS' || $Mode eq 'BOTH'){
    eval{
      print CHAINLOG '%' x 80, 
      "\n Here is logfile for HPSS\n",'%' x 80, "\n\n";
      
      $ftp = Net::FTP->new($host) or die "Cannot connect to $host\n";
      if(!defined $ftp){
	print CHAINLOG "Cannot connect to $host\n";
	die "Cannot connect\n";
      }
      
      print CHAINLOG "Connection o.k.\n";
      
      $status = $ftp->login();
      if(!$status) {
	print CHAINLOG "Could not log in\n";
	die "Couldnt log in\n";
      }
      print CHAINLOG "Login o.k.\n";
      
      # go to $hpss_top_dir; create it if it doesnt exist.
      #---
      my $status = $ftp->cwd($hpss_top_dir);
      if(!$status){
	print CHAINLOG "$hpss_top_dir doesnt exist, creating it\n";
	$ftp->mkdir($hpss_top_dir,1);
	$ftp->cwd($hpss_top_dir) or do{
	  print CHAINLOG "Couldn't create $hpss_top_dir\n";
	  die "couldnt create $hpss_top_dir\n";
	};
      }
 
      print CHAINLOG "Here is listing of $hpss_top_dir...\n";
      
      my @lines = $ftp->dir();
      for my $line (@lines) { 
	print CHAINLOG "$line\n";
      }
      #---
      # go to hpss data dir, create it if it doesn't exist
  
      $ftp->cwd($hpss_data_dir) or do{
	print CHAINLOG "Directory $hpss_data_dir doesn't exist, creating it...\n";
	$ftp->mkdir($hpss_data_dir,1);
	$ftp->cwd($hpss_data_dir) or do{
	  print CHAINLOG "Couldn't create $hpss_data_dir, goodbye\n";
	  die "Couldnt create $hpss_data_dir\n";
	};
      };
      #---
      # copy setup file to data directory
      copy ($self->SetupFile(),$self->DataDir());
      
      # go to local data directory, get list of files to be shipped to HPSS
      my $now = cwd();
      my $data_dir = $self->DataDir();
      chdir $data_dir;
      
      opendir (DIR, $data_dir);
      my @files = grep {/\.dst\.root|\.fz|\.geant\.root|\.hbook|\.hist\.root|\.event\.root|\.MuDst\.root|\.runco\.root|\.setup$/ } readdir (DIR);
      closedir DIR;
      
      #---
      # write files to HPSS, check that they arrived
      
      print CHAINLOG "\n*** Putting files to HPSS from directory $data_dir\n";
      
      $ftp->binary() or do{
	print CHAINLOG "Could not switch to binary\n";
	die "Could not switch to binary\n";
      };

      for my $file (@files){
	print CHAINLOG "Putting file $file...\n";
	$ftp->put($file) or do{
	  print CHAINLOG "Error during ftp->put for file $file\n";
	  #next;
	  die "Error during put for $file\n";
	};
	
	# check file sizes
	my $hpss_filename = basename($file);

	# pmj 23/3/01: bug in Net:FTP->size, do a workaround...
	# my $hpss_size = $ftp->size($hpss_filename);

	my @dir_lines = $ftp->dir();
	my $hpss_size = -1;
	foreach my $line (@dir_lines){
	  $line =~ /$hpss_filename/ or next;
	  $line =~ /^\S+\s+\S+\s+\S+\s+\S+\s+(\S+)/;
	  $hpss_size = $1;
	  last;
	}
	#--- end of workaround

	my $inode = stat($file);
	my $local_size = $inode->size;

	if ($hpss_size != $local_size){
	  print CHAINLOG "Error in hpss filesize for $file: local filesize = $local_size,",
	  "hpss filesize =  $hpss_size\n";
	  die "Wrong size : local $local_size, hpss $hpss_size\n";
	}
      }
      
      print CHAINLOG "\n *** Done with ftp->put\n";
      
      #---
      #chdir $now;
      #---
      
    }
  };

  #---------------------------------------------------------
  # need to write log file to HPSS **after** it is closed

  my $data_dir = $self->DataDir();
  
  if($Mode eq 'HPSS' || $Mode eq 'BOTH'){
    #
    # something went wrong
    #    
    if($@){
      my $error = "ERROR in hpss!\n";
      $error .= "See files (if any) on $hpss_data_dir\n";
      print CHAINLOG $error;
      print $error;
      close CHAINLOG;
      
      #
      # move everything to SaveDir()
      #
      my $save_dir = $self->SaveDir();
      my $command  = "\\mv -f $data_dir $save_dir";
      print "Moving the data dir to save dir : $save_dir...\n";
      my $status = system($command);
      if($status==0){
	print "...done\n";
      }
      else{
	print "Error in mv\n";
      }
      #
      # send mail to be added later...
      #

      die "Exiting Chain...\n"; # for the lsf exit status
    }
    #
    # hpss shipping was ok
    #
    else{
      close CHAINLOG;

      # send the $chain_log
      if(defined $ftp){
	$ftp->put($chain_log) or print "Could not ftp->put chainlog\n";
	$ftp->quit() or warn "Could not quit ftp\n";
      }
      
      my $message = "Shipping to HPSS successful\n";
      $message .= "Files on hpss in : $hpss_data_dir\n";
      print $message;
    }
    #
    # always clean up scratch if HPSS mode
    #
    
    END{ 
      if($MODE eq 'HPSS'){
	CleanUp();
      }
      # 02/07/03 - BUM remove unnecessary files in 'BOTH' mode
      elsif($MODE eq 'BOTH'){
	CleanUpDisk();
      }
    }
    
  }
  else { # DISK mode.
    close CHAINLOG;
  }
  
}

sub CleanUp{
  if (-e $DATADIR){
    print "Removing data_dir: $Chain_object::DATADIR...\n";
    my $files = rmtree $DATADIR;
    if(!-e $DATADIR) { 
      print "...done\n"; 
    }
    else             { 
      print "Cannot remove data_dir"; 
    }
  }
}

sub CleanUpDisk{
  print "Removing unnecessary files in $DATADIR...\n";
  opendir (DIR, $DATADIR) or die "$DATADIR : $!\n";
  while(my $file=readdir(DIR)){
    next if $file =~ /^\./;
    my $fullName = "$DATADIR/$file";
    unlink $fullName if (-f $fullName && $fullName !~ /\.root$|\.setup$/);
    rmtree $fullName if (-d $fullName);
  }
  closedir DIR;
  print "...done with attempt\n";

}
