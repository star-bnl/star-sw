#! /usr/local/bin/perl -w 
#
# parse production log file

#=========================================================
use strict;
use Sys::Hostname;
my $hostname     = hostname();
my $mdir_log      = "/star/rcf/disk00001/star/mdc3/log/";
my $mdir_sum      = "/star/rcf/disk00001/star/mdc3/sum/"; 
my @dir_ext      = ("tfs","daq");  
my @set ;
my @list; 
my @list_sum;     
my $job_log;
my $dummy;
my $num_event = 0;
my $file_sum;
my $dir_lg       = "../log";
my $name_log;
my $f_flag = 0;
my $dir_log;
my $dir_sum;
#=========================================================
my $ii =0;

#for ($ii =0; $ii<2; $ii++) {
 $ii = 0;
$dir_log = $mdir_log . $dir_ext[$ii];
$dir_sum = $mdir_sum . $dir_ext[$ii];

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
#}
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
#  my $num_event;
  my $line; 
  my $jrun = "Run not completed";
  my $segmentation_violation;
  my $break_buss;
  my $Err_messg = "none";
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
  my $no_event = 0;  
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
  my $no_xivertices;
  my $no_ssd_hits;
  my $no_knvertices;
  my $no_prtracks; 
  my $tot_tracks = 0;
  my $tot_vertices = 0;
  my $tot_tpc_hits = 0;
  my $tot_svt_hits = 0;
  my $tot_ftpc_hits = 0;
  my $tot_prtracks = 0;
  my $tot_knvertices = 0;
  my $tot_ssd_hits = 0;
  my $tot_xivertices = 0;
  my $avr_tracks;
  my $avr_vertices;
  my $avr_prtracks;
  my $avr_knvertices;
  my $avr_xivertices;
  my $avr_ssd_hits;
  my $avr_tpc_hits;
  my $avr_svt_hits;
  my $avr_ftpc_hits;
  my @word_tr;
  my $i;
  my $noEvts = 0;
  my $first_evts = 0;
  my $last_evts = 0; 
  my @nparts;
  my $last_maker = 0;
  my @size_line;
  my $ij = 0;
  my @line_tag;
  my $tag_flag = 0;
  my @tag_word;
  my $last_tag_line;
  my $last_tag = "last";
  my $node_name = "n/\a";
  my $first_line;
  my $star_level = "n/\a";
  my $root_level = "n/\a"; 
  my $exmessg = "none";

  #---------------------------------------------------------
  
  # parse beginning of file
  
  $first_line = $logfile[1];
   chop $first_line ;
  foreach $line (@logfile) {
     chop $line ;
      $num_line++;
# get STAR_LEVEL and ROOT_LEVEL
    
    if ( $line =~ /QAInfo:You are using STAR_LEVEL/) {
       @nparts = split (" ", $line);
      $star_level = $nparts[5];
      $root_level = $nparts[9];    
     }
    
    # get library version
    if ( (! $lib_version) && $line =~ /={3} You are in (\w+)/ ) {
      $lib_version = $1;
      $record_run_options = 1;
    }
    
    # get input file name
    if ( (! $input_fn) && $line =~ /Input file name = (\w+)/ ) {
#     if ( (! $input_fn) && $line =~ /Input file name/) {
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
    if ( $line =~ /QAInfo: Done with Event/ ) {
      @nparts = split ( "/",$line);
      $noEvts = $nparts[2];
      $last_evts = substr($noEvts,4) + 0; 
      if ($no_event eq 0) {
      $first_evts = $last_evts;
    }
      $no_event++;
  } 
    # get number of tracks, vertices and hits

     if ($line =~ /QAInfo: StAnalysisMaker/ ) {

           my  $string = $logfile[$num_line];
             @word_tr = split /:/,$string;
              $no_tracks = $word_tr[2];
             $tot_tracks += $no_tracks; 
#              print $word_tr[2], $no_tracks, "\n";
              $string = $logfile[$num_line + 1];
             @word_tr = split /:/,$string;
             $no_prtracks = $word_tr[2]; 
             $tot_prtracks += $no_prtracks;
             $string = $logfile[$num_line + 2];
             @word_tr = split /:/,$string;
             $no_vertices = $word_tr[2]; 
             $tot_vertices += $no_vertices;
             $string = $logfile[$num_line + 3];
             @word_tr = split /:/,$string;
             $no_xivertices = $word_tr[2]; 
             $tot_xivertices += $no_xivertices;
             $string = $logfile[$num_line + 4];
             @word_tr = split /:/,$string;
             $no_knvertices = $word_tr[2]; 
             $tot_knvertices += $no_knvertices;
             $string = $logfile[$num_line + 5];
             @word_tr = split /:/,$string;
             $no_tpc_hits = $word_tr[2];
             $tot_tpc_hits += $no_tpc_hits;
             $string = $logfile[$num_line + 6];
             @word_tr = split /:/,$string;
             $no_svt_hits = $word_tr[2]; 
             $tot_svt_hits += $no_svt_hits;
             $string = $logfile[$num_line + 7];
             @word_tr = split /:/,$string;
             $no_ssd_hits = $word_tr[2];
             $tot_ssd_hits += $no_ssd_hits; 
             $string = $logfile[$num_line + 8];
             @word_tr = split /:/,$string;
             $no_ftpc_hits = $word_tr[2];
             $tot_ftpc_hits += $no_ftpc_hits; 
           
 }   
    # check if job crashed due to break_buss_error
     if($line =~ /bus error/) {
         $Err_messg = "Break bus error";
       }


    # check if job crashed due to segmentation violation
    if ($line =~ /segmentation violation/) {
             $Err_messg = "segmentation violation";
    }

    $previous_line = $line;
    
    #check if job is completed
    if ( $line =~ /Run completed/) {
          
          $jrun = "Done";      
	}
    if ( $line =~ /Finished job execution/) {
      @word_tr = split(" ",$line);
       $node_name = $word_tr[11];    
       $start_line = $first_line . " " . $node_name;       
	} 

     }
##----------------------------------------------------------------------------
# parse error log file

   my @err_out;
   my $mline;
   my $jLog_err;
 
    $jLog_err = $filename;
    $jLog_err =~ s/log/err/g;

     @err_out = `tail -200 $jLog_err`;
  foreach $mline (@err_out){
          chop $mline;
       if ( $mline =~ /No space left on device/)  {
        $Err_messg = "No space left on device";
     } 
	elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $exmessg = $mline;
      }
     elsif ($mline =~ /Stale NFS file handle/) {
  
      $exmessg = "Stale NFS file handle";
 }       
       if ( $mline =~ /Assertion/ & $mline =~ /failed/)  {
        $Err_messg = "Assertion failed";
     } 
   }
  
  #--------------------------------------------------------------------------
   $num_event = $no_event;   
 
  # output header info
  
  print '=' x 80, "\n";

  print $start_line, "\n"; 
   print '-' x 80, "\n";

  ! defined($lib_version) and $lib_version = $not_found_string;
  print ("Library version : ", $lib_version, "\n");

   print ("STAR_LEVEL : ", $star_level, "\n");

   print ("ROOT_LEVEL : ", $root_level, "\n");
  

   ! defined($input_fn) and $input_fn = $not_found_string;
   print ("Input file : ", $input_fn, "\n");
   print '=' x 80, "\n";
  
   ! defined($run_option_string) and $run_option_string = $not_found_string;
  
   print (">>>>>>>>>>>>  Run options: <<<<<<<<<<<< \n", $run_option_string, "\n");
   print '-' x 80, "\n";
  
 
   print ("Number of Events Done: ", $num_event, "\n");
    print ("First event no. : ", $first_evts, "\n");
    print ("Last event no. : ", $last_evts, "\n");

   print '-' x 80, "\n"; 

   if($jrun eq "Run not completed" & $Err_messg ne "none") {
     $jrun = $Err_messg ;
   }
   print("Job status:", $jrun, " \n");
   print("Extra error message: ", $exmessg, "\n");

   print '=' x 80, "\n";
   print(">>>>>>>>>> Maker's tag <<<<<<<<<<\n");
   print '=' x 80, "\n"; 

   for ($i = 0; $i <= $last_tag_line; $i++){
   print $line_tag[$i], "\n";
 }
#  if ( !defined($segmentation_violation) and !defined ($break_buss))  {
     
   print '=' x 80, "\n";
   print(">>> Average number of tracks, vertices and hits found <<<\n");
   print '=' x 80, "\n";

  if( $num_event ne 0 & $last_maker ne 0)  {
   $avr_tracks    = $tot_tracks/$num_event;
   $avr_vertices  = $tot_vertices/$num_event;
   $avr_tpc_hits  = $tot_tpc_hits/$num_event;
   $avr_svt_hits  = $tot_svt_hits/$num_event;
   $avr_ftpc_hits = $tot_ftpc_hits/$num_event;
   $avr_prtracks  = $tot_prtracks/$num_event;
   $avr_knvertices = $tot_knvertices/$num_event;
   $avr_xivertices = $tot_xivertices/$num_event;
   $avr_ssd_hits   = $tot_ssd_hits/$num_event;
  
    printf ("QAinfo: Average number of tracks:          %10i \n",  $avr_tracks); 
    printf ("QAinfo: Average number of primary tracks:  %10i \n",  $avr_prtracks); 
    printf ("QAinfo: Average number of vertices:        %10i \n",  $avr_vertices );
    printf ("QAinfo: Average number of Xi vertices:     %10i \n",  $avr_xivertices );
    printf ("QAinfo: Average number of Kink vertices:   %10i \n",  $avr_knvertices ); 
    printf ("QAinfo: Average number of TPC hits:        %10i \n",  $avr_tpc_hits ); 
    printf ("QAinfo: Average number of SVT hits:        %10i \n",  $avr_svt_hits );
    printf ("QAinfo: Average number of SSD hits:        %10i \n",  $avr_ssd_hits ); 
    printf ("QAinfo: Average number of FTPC hits:       %10i \n",  $avr_ftpc_hits ); 

 }
   print '=' x 80, "\n";
   print(">>> Average memory usage for each package at the time of execution <<<\n");
   print '=' x 80, "\n";

   for ($i = 0; $i < $last_maker; $i++){

   $msize_aver[$i] = $maker_size[$i]/($num_event * 1000);
   printf("Package   %s     Memory size =  %10.3f  MB; \n", $maker_name[$i], $msize_aver[$i]);
    }
  }
#}

 #--------------------------------------------------------------------------
  
 # parse end of file
 sub timestamp($) {
   my $job_log = $_[0];   
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

   if ($num_event ne 0) {
 @cpu_output = `tail -250 $job_log`;
  foreach $end_line (@cpu_output){
          chop $end_line;
   if ($end_line =~ /seconds Cpu Time/) {
     @part = split (" ", $end_line); 
    if($part[0] ne "QAInfo:" and $part[2] =~ /Real/) {        
     @myword = split /:/, $end_line; 
     $Maker = $myword[1]; 
     @words = split(" ",$myword[2]);
     $real_time = $words[3] / $num_event;
     $cpu_time = $words[8] / $num_event;
 
#    print ($Maker, ": Real Time = ", $real_time," seconds;   Cpu Time = ", $cpu_time, " seconds; \n");
   printf ("%s : Real Time =  %10.3f seconds;   Cpu Time = %10.3f seconds;\n", $Maker, $real_time, $cpu_time);
   }
     elsif($part[1] =~ /Total:/) {
     $Maker = $part[1] . " " . $part[2];
     @myword = split /:/, $end_line;    
     @words = split(" ",$myword[3]);
     $real_time = $words[3] / $num_event;
     $cpu_time = $words[8] / $num_event;
   printf ("%s : Real Time =  %10.3f seconds;   Cpu Time = %10.3f seconds;\n", $Maker, $real_time, $cpu_time);
   }

     if ($end_line =~ /Total: bfc/){
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
