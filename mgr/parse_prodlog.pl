#! /usr/local/bin/perl -w 

# parse production log file

#=========================================================
use strict;
use Sys::Hostname;
my $hostname     = hostname();
my $dir_log      = "/disk00001/star/MDC3/test";
my $dir_sim      = "../sum";   
my @set ;
my @list;      
my $job_log;
my $dummy;
my $file_sum;
#=========================================================
chdir $dir_log;
@list = `ls *evts`;
foreach my $file (@list) {
       my  $ltime = `mod_time $file`;
#           print $ltime; 
           if( $ltime > 1200){ 
             parse_log($file);
             timestamp($file);
         $dummy = `mv $file_sum ../sum`;  
           }             
    }
exit(0);
#==========================================================
 sub mod_time($) { 
my $atime;   # Last access time since the epoch
my $mtime;   # Last modify time since the epoch
my $ctime;   # Inode change time (NOT creation time!) since the epoch
my $now;     # current time 
my $dev;
my $ino;
my $mode;
my $nlink;
my $uid;
my $gid;
my $rdev;
my $size;
my $blksize;
my $blocks;
my $dtime;
my $file_name =  $ARGV[0];

$now = time;
($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $file_name;
printf ($mtime);
$dtime = $now - $mtime;
printf  ($dtime) ;
}


#===================================================================
sub parse_log($) {

  my $filename = $_[0];
  my $tag = "_sum";
     chop $filename;
  $file_sum = $filename . $tag;
  my $input_fn;
  my $command_string;
  my $lib_version;  
  my $record_run_options = 0;
  my $run_option_length = 0;
  my $run_option_string;
  my $error_flag = 0;
  my $start_line = "";
  my $last_event;
  my $line; 
  my $jrun = "Run not completed";
  my $segmentation_violation;
  
  my $previous_line = "";
  #----------------------------------------------------------

  my $not_found_string = "Not found";

  #----------------------------------------------------------
  
  open (LOGFILE, $filename) or die "cannot open $filename: $!\n";
  open (STDOUT, ">$file_sum");
  
  #---------------------------------------------------------
  
  # dump contents of log file to array for parsing
  my @logfile = <LOGFILE>;
  
  #---------------------------------------------------------
  
  # parse beginning of file
  
  $start_line = $logfile[0];
 
  foreach $line (@logfile) {
     chop $line ;
  $error_flag = 0;
    
    # all done with beginning of file?
#    last if $line =~ /\*{3} Call St_db_Maker::Init\(\) \*{3}/;

    # get command string
    if ( (! $command_string) && $line =~ /Processing (bfc\.C.*)/ ) {
      $command_string = $1;
      # strip trailing ellipsis...
      $command_string =~ s/\.{3}//;
    }
    
    # get library version
    if ( (! $lib_version) && $line =~ /={3} You are in (\w+)/ ) {
      $lib_version = $1;
      $record_run_options = 1;
    }
    
    # get input file name
    if ( (! $input_fn) && $line =~ /Input file name = (\S+).*process = ([0-9]+)/ ) {
      $input_fn = $1;
      $record_run_options = 0;
    }
    
    # concatenate run options; skip first time through when lib_version is detected
    $record_run_options and $run_option_length and $run_option_string .= $line."\n";
    $record_run_options and $run_option_length++;

   
    # get last event number
    if ( $line =~ /Done with Event no. ([0-9]+)\W+([0-9]+)/ ) {
      $last_event = $1;

    } 
    # check if job crashed
    if ($line =~ /segmentation violation/) {
             $segmentation_violation = $previous_line;
    }

    $previous_line = $line;
    
    #check if job is completed
    if ( $line =~ /Run completed NULL/) {
          
          $jrun = "Done";      
	}

  }

  #--------------------------------------------------------------------------
  
  # output header info
  
  print '=' x 80, "\n";

  print $start_line, "\n"; 
   print '-' x 80, "\n";

  ! defined($lib_version) and $lib_version = $not_found_string;
  print ("Library version:", $lib_version, "\n");


  ! defined($command_string) and $command_string = $not_found_string;
  print ("Command string:", $command_string, "\n");


   ! defined($input_fn) and $input_fn = $not_found_string;
   print ("Input file:", $input_fn, "\n");
   print '=' x 80, "\n";
  
   ! defined($run_option_string) and $run_option_string = $not_found_string;
  
   print (">>>>>>>>>>>>  Run options: <<<<<<<<<<<< \n", $run_option_string, "\n");
   print '-' x 80, "\n";
  
   ! defined($last_event) and $last_event = $not_found_string; 
 
   print ("Number of Events Done:  ", $last_event, "\n");

   print '-' x 80, "\n"; 

   
   if ( defined($segmentation_violation) ){

   print( "***Segmentation violation found:  ", $segmentation_violation, "***\n");

 }
   print(">>>>>>>>>>  Job status:  ", $jrun, "  <<<<<<<<<<\n");
         
}
 #--------------------------------------------------------------------------
  
 # parse end of file
 sub timestamp($) {
   my $job_log = $_[0];
   my $no_events;    
   my @cpu_output;
   my @part;
   my $end_line;
   my $Maker;
   my @words;
   my $real_time;
   my $cpu_time;

    print '=' x 80, "\n";
    print ">>>>>>>>>>>>>>>  Average CPU TIME per EVENT for Makers <<<<<<<<<<<<<<<\n";
    print '=' x 80, "\n";    
  @cpu_output = `tail -70 $job_log`;
  foreach $end_line (@cpu_output){
   if ($end_line =~ /Done with Event no. ([0-9]+)\W+([0-9]+)/ ) {
      $no_events = $1;
    } 
   if ($end_line =~ /Cpu Time/) {
     @part = split /:/, $end_line;
     $Maker = $part[0];
     @words = split(" ",$part[1]);
     $real_time = $words[3] / $no_events;
     $cpu_time = $words[8] / $no_events;

#    print ($Maker, ": Real Time = ", $real_time," seconds;   Cpu Time = ", $cpu_time, " seconds; \n");
    printf ("%s : Real Time =  %10f seconds;   Cpu Time = %10f seconds;\n", $Maker, $real_time, $cpu_time);
     if ($end_line =~ /bfc/){
        last;
     } 
    }
   }
   print '=' x 80, "\n";
   print "End of file";
   close (STDOUT);
 }
#=====================================================================================
