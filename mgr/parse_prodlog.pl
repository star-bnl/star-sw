#! /usr/local/bin/perl -w 

# parse production log file

#=========================================================
use strict;
use Sys::Hostname;
my $hostname     = hostname();
my $dir_log      = "/disk00001/star/MDC3/test";
my $dir_sim      = "../sum";
my $dir_archive  = "../archive";   
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
           if( $ltime > 1200){ 
             parse_log($file);
             timestamp($file);
         $dummy = `mv $file_sum ../sum`;
         $dummy = `mv $file ../archive`;   
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
  my @size_line;
  my $num_line = 0; 
  my @maker_size = 0;
  my @mem_words;
  my $num_maker = 0;
  my @maker_name;
  my @msize_aver = 0; 
  my @mymaker; 
  my $i;
  my $last_maker;
  #---------------------------------------------------------
  
  # parse beginning of file
  
  $start_line = $logfile[0];
 
  foreach $line (@logfile) {
     chop $line ;
      $num_line++;
  $error_flag = 0;
    

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


    # get memory size for each Maker
     if( $line =~ /EndMaker/){
        $size_line[0] = $logfile[$num_line + 1];   
        $size_line[1] = $logfile[$num_line + 2]; 
       @mem_words = split(" ",$size_line[0]);  
       $maker_size[$num_maker] += $mem_words[4];
       @mem_words = split(" ",$size_line[1]);
       @mymaker = split ("::",$mem_words[1]);
 
       $maker_name[$num_maker] = $mymaker[0];
       $num_maker++;
       if( $line =~ /tree.EndMaker/){
         $last_maker = $num_maker;
         $num_maker = 0; 
       }
      }
  
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
    if ( $line =~ /Run completed/) {
          
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

   print '=' x 80, "\n";
   print(">>> Average memory usage for each Maker at the time of execution <<<\n");
   print '=' x 80, "\n";

   for ($i = 1; $i < $last_maker; $i++){

   $msize_aver[$i] = $maker_size[$i]/($last_event * 1000);
   printf("%s :       Memory size =  %10.3f  MB; \n", $maker_name[$i], $msize_aver[$i]);
   }        
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
    printf ("%s : Real Time =  %10.3f seconds;   Cpu Time = %10.3f seconds;\n", $Maker, $real_time, $cpu_time);
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
