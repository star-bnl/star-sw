#! /usr/star/bin/perl -w 

# parse production log file

#=========================================================
use strict;
use Sys::Hostname;
my $hostname     = hostname();
my $dir_log      = "/disk00001/star/prod4/log/tfs";
my $dir_sum      = "../../sum/tfs";   
my @set ;
my @list;      
my $job_log;
my $dummy;
my $file_sum;
#my $dir_lg       = "../log";
my $name_log;
my $f_flag = 0;
my $pwd;
my $filesm;
#=========================================================
chdir $dir_log;
@list = `ls *log`;

foreach my $file (@list) {
        my $ltime = `mod_time $file`;
           if( $ltime > 3600){
                chop $file;
               $f_flag = 0; 
              $file_sum = $file;
              $file_sum =~ s/.log//g;
              $file_sum = $file_sum . ".sum";
        chdir $dir_sum;
         if(-f $file_sum ) {
              $f_flag = 1;
	    }
         chdir $dir_log;
              if($f_flag != 1) {  
             parse_log($file);
             timestamp($file);
            $name_log = $file; 
         $dummy = `mv $file_sum $dir_sum`;
#         $dummy = `mv $name_log $dir_lg`;   
           }             
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
  my $tag = ".sum";
#  my $file_sm;
#     chop $filename;
#  $file_sm = $filename;
#  $file_sm =~ s/.log//g;
#  $file_sum = $file_sm . $tag;
  my $input_fn;
  my $lib_version;  
  my $record_run_options = 0;
  my $run_option_length = 0;
  my $run_option_string;
  my $start_line = "";
  my $num_event;
  my $line; 
  my $jrun = "Run not completed";
  my $segmentation_violation;
  my $break_buss;
  my $previous_line = "";
  #----------------------------------------------------------

  my $not_found_string = "Not found";

  #----------------------------------------------------------
  
  open (LOGFILE, $filename ) or die "cannot open $filename: $!\n";
  
  open (STDOUT, ">$file_sum");

  #---------------------------------------------------------
  
  # dump contents of log file to array for parsing
  my @logfile = <LOGFILE>;
  my $num_line = 0; 
  my @maker_size = 0;
  my @mem_words;
  my $num_maker = 0;
  my @maker_name;
  my @msize_aver = 0; 
  my $mymaker; 
  my $no_tracks; 
  my $no_vertices;
  my $no_tpc_hits;
  my $no_svt_hits;
  my $no_ftpc_hits;
  my $tot_tracks = 0;
  my $tot_vertices = 0;
  my $tot_tpc_hits = 0;
  my $tot_svt_hits = 0;
  my $tot_ftpc_hits = 0;
  my $avr_tracks;
  my $avr_vertices;
  my $avr_tpc_hits;
  my $avr_svt_hits;
  my $avr_ftpc_hits;
  my @word_tr;
  my $i;
  my $last_maker;
  my @size_line;
  my $ij = 0;
  my @line_tag;
  my $tag_flag = 0;
  my @tag_word;
  my $last_tag_line;
  my $last_tag = "last";
  #---------------------------------------------------------
  
  # parse beginning of file
  
  $start_line = $logfile[1];
 
  foreach $line (@logfile) {
     chop $line ;
      $num_line++;
    
    # get library version
    if ( (! $lib_version) && $line =~ /={3} You are in (\w+)/ ) {
      $lib_version = $1;
      $record_run_options = 1;
    }
    
    # get input file name
    if ( (! $input_fn) && $line =~ /QAInfo:Input file name = (\w+)/ ) {
      $input_fn = $1;
      $record_run_options = 0;
    }
    
    # concatenate run options
    $record_run_options and $run_option_length and $run_option_string .= $line."\n";
    $record_run_options and $run_option_length++;

    # print tag for each Maker
     if ( $line =~ /built/) {      
      if($tag_flag != 1) { 
      @tag_word = split(" ", $line);
      if( $tag_word[0] eq "QAInfo:StTreeMaker") {
         $tag_flag = 1;
        $last_tag_line = $ij;
       }
        $line_tag[$ij] = $line;
        $ij++;
     } 
    }
    # get memory size for each Maker
     if ($num_line > 100){
     if( $line =~ /EndMaker/){
       @size_line = split(" ",$line); 
       if($size_line[0] eq "QAInfo:" and $size_line[5] eq "starreco") {
     
        $maker_size[$num_maker] += $size_line[9];
         $mymaker = $size_line[3];
       $maker_name[$num_maker] = $mymaker;
       $num_maker++;
       if( $mymaker eq "tree:"){
         $last_maker = $num_maker;
         $num_maker = 0; 
       }
      }
     }
   }
    # get  number of events
    if ( $line =~ /QAInfo: Done with Event no. ([0-9]+)\W+([0-9]+)/ ) {
      $num_event = $1;
  } 
    # get number of tracks, vertices and hits

     if ($line =~ /StInfo: QAInfo: StAnalysisMaker/ ) {

           my  $string = $logfile[$num_line];
             @word_tr = split /:/,$string;
             $no_tracks = $word_tr[3];
             $tot_tracks += $no_tracks; 
#              print $word_tr[2], $no_tracks, "\n";
              $string = $logfile[$num_line + 1];
             @word_tr = split /:/,$string;
             $no_vertices = $word_tr[3]; 
             $tot_vertices += $no_vertices;
             $string = $logfile[$num_line + 2];
             @word_tr = split /:/,$string;
             $no_tpc_hits = $word_tr[3];
             $tot_tpc_hits += $no_tpc_hits;
             $string = $logfile[$num_line + 3];
             @word_tr = split /:/,$string;
             $no_svt_hits = $word_tr[3]; 
             $tot_svt_hits += $no_svt_hits;
             $string = $logfile[$num_line + 4];
             @word_tr = split /:/,$string;
             $no_ftpc_hits = $word_tr[3];
             $tot_ftpc_hits += $no_ftpc_hits; 
           
 }   
    # check if job crashed due to break_buss_error
     if($line =~ /buss error/) {
         $break_buss = "Break buss error";
       }


    # check if job crashed due to segmentation violation
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

  
   ! defined($input_fn) and $input_fn = $not_found_string;
   print ("Input file:", $input_fn, "\n");
   print '=' x 80, "\n";
  
   ! defined($run_option_string) and $run_option_string = $not_found_string;
  
   print (">>>>>>>>>>>>  Run options: <<<<<<<<<<<< \n", $run_option_string, "\n");
   print '-' x 80, "\n";
  
   ! defined($num_event) and $num_event = $not_found_string; 
 
   print ("Number of Events Done:  ", $num_event, "\n");

   print '-' x 80, "\n"; 

   
   if ( defined($segmentation_violation) ){

   print( "Segmentation violation found:  *** ", "\n");

 }
  if ( defined($break_buss) ){

   print( "Break *** buss error: *** ", "\n");

 } 

  print("Job status:  ", $jrun, " \n");

   print '=' x 80, "\n";
   print(">>>>>>>>>> Maker's tag <<<<<<<<<<\n");
   print '=' x 80, "\n"; 

   for ($i = 0; $i <= $last_tag_line; $i++){
   print $line_tag[$i], "\n";
 }

   print '=' x 80, "\n";
   print(">>> Average number of tracks, vertices and hits found <<<\n");
   print '=' x 80, "\n";
 
   $avr_tracks    = $tot_tracks/$num_event;
   $avr_vertices  = $tot_vertices/$num_event;
   $avr_tpc_hits  = $tot_tpc_hits/$num_event;
   $avr_svt_hits  = $tot_svt_hits/$num_event;
   $avr_ftpc_hits = $tot_ftpc_hits/$num_event;
    printf ("QAinfo: number of tracks:    %10i \n",  $avr_tracks); 
    printf ("QAinfo: number of vertices:  %10i \n",  $avr_vertices ); 
    printf ("QAinfo: number of TPC hits:  %10i \n",  $avr_tpc_hits ); 
    printf ("QAinfo: number of SVT hits:  %10i \n",  $avr_svt_hits ); 
    printf ("QAinfo: number of FTPC hits: %10i \n",  $avr_ftpc_hits ); 

   print '=' x 80, "\n";
   print(">>> Average memory usage for each package at the time of execution <<<\n");
   print '=' x 80, "\n";

   for ($i = 0; $i < $last_maker; $i++){

   $msize_aver[$i] = $maker_size[$i]/($num_event * 1000);
}
   printf("Package   %s              Memory size =  %10.3f  MB; \n", $maker_name[0], $msize_aver[0]);
   printf("Package   %s           Memory size =  %10.3f  MB; \n", $maker_name[1], $msize_aver[1]);
   printf("Package   %s           Memory size =  %10.3f  MB; \n", $maker_name[2], $msize_aver[2]);
   printf("Package   %s         Memory size =  %10.3f  MB; \n", $maker_name[3], $msize_aver[3]);
   printf("Package   %s        Memory size =  %10.3f  MB; \n", $maker_name[4], $msize_aver[4]);
   printf("Package   %s       Memory size =  %10.3f  MB; \n", $maker_name[5], $msize_aver[5]);
   printf("Package   %s      Memory size =  %10.3f  MB; \n", $maker_name[6], $msize_aver[6]);
   printf("Package   %s     Memory size =  %10.3f  MB; \n", $maker_name[7], $msize_aver[7]);
   printf("Package   %s             Memory size =  %10.3f  MB; \n", $maker_name[8], $msize_aver[8]);
   printf("Package   %s             Memory size =  %10.3f  MB; \n", $maker_name[9], $msize_aver[9]);  
   printf("Package   %s             Memory size =  %10.3f  MB; \n", $maker_name[10], $msize_aver[10]);
   printf("Package   %s           Memory size =  %10.3f  MB; \n", $maker_name[11], $msize_aver[11]);
   printf("Package   %s         Memory size =  %10.3f  MB; \n", $maker_name[12], $msize_aver[12]);
   printf("Package   %s              Memory size =  %10.3f  MB; \n", $maker_name[13], $msize_aver[13]);
   printf("Package   %s              Memory size =  %10.3f  MB; \n", $maker_name[14], $msize_aver[14]);
   printf("Package   %s            Memory size =  %10.3f  MB; \n", $maker_name[15], $msize_aver[15]);
   printf("Package   %s          Memory size =  %10.3f  MB; \n", $maker_name[16], $msize_aver[16]);
   printf("Package   %s             Memory size =  %10.3f  MB; \n", $maker_name[17], $msize_aver[17]);
   printf("Package   %s    Memory size =  %10.3f  MB; \n", $maker_name[18], $msize_aver[18]);
   printf("Package   %s        Memory size =  %10.3f  MB; \n", $maker_name[19], $msize_aver[19]);
   printf("Package   %s              Memory size =  %10.3f  MB; \n", $maker_name[20], $msize_aver[20]);
   printf("Package   %s            Memory size =  %10.3f  MB; \n", $maker_name[21], $msize_aver[21]);  
  }
 #--------------------------------------------------------------------------
  
 # parse end of file
 sub timestamp($) {
   my $job_log = $_[0];
   my $no_event ;   
   my @cpu_output;
   my @part;
   my @myword; 
   my $end_line;
   my $Maker;
   my @words;
   my $real_time;
   my $cpu_time;
   my $num_ln = 0;
   my $jj;
    print '=' x 80, "\n";
    print ">>>>>>>>>>>>>>>  Average CPU TIME per EVENT for Makers <<<<<<<<<<<<<<<\n";
    print '=' x 80, "\n";    

 @cpu_output = `tail -90 $job_log`;
  foreach $end_line (@cpu_output){
          chop $end_line;
    if ($end_line =~ /Done with Event/ ) {
       @words = split (" ",$end_line);
      $no_event = $words[5]; 
    }
     
   if ($end_line =~ /Cpu Time/) {
     @part = split (" ", $end_line); 
    if($part[0] ne "QAInfo:" and $part[2] =~ /Real/) {        
     @myword = split /:/, $end_line; 
     $Maker = $myword[1]; 
     @words = split(" ",$myword[2]);
     $real_time = $words[3] / $no_event;
     $cpu_time = $words[8] / $no_event;
 
#    print ($Maker, ": Real Time = ", $real_time," seconds;   Cpu Time = ", $cpu_time, " seconds; \n");
   printf ("%s : Real Time =  %10.3f seconds;   Cpu Time = %10.3f seconds;\n", $Maker, $real_time, $cpu_time);
     if ($end_line =~ /bfc/){
        last;
   }
  }
 }   
} 
  print '=' x 80, "\n";
   print "End of file";
   close (STDOUT);
 }
#=====================================================================================
