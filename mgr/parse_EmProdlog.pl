#! /opt/star/bin/perl -w 
#
# parse production log file

#=========================================================
use Class::Struct;
use File::Basename;
#use File::Find;
use Sys::Hostname;

#my $debugOn=0;

my $hostname     = hostname();
my @dir_log      = (
#                    "/star/rcf/data06/reco/embedding_alamhipt/P00hg/log/daq",
#                    "/star/rcf/data06/reco/embedding_alamlopt/P00hg/log/daq",
#                    "/star/rcf/data06/reco/embedding_lamhipt/P00hg/log/daq",
#                    "/star/rcf/data06/reco/embedding_lamlopt/P00hg/log/daq",
                    "/star/rcf/data07/reco/embedding_pbar/P00hg/log/daq",
);

my @dir_sum      = (
#                     "/star/rcf/data06/reco/embedding_alamhipt/P00hg/sum/daq",
#                     "/star/rcf/data06/reco/embedding_alamlopt/P00hg/sum/daq",
#                     "/star/rcf/data06/reco/embedding_lamhipt/P00hg/sum/daq",
#                     "/star/rcf/data06/reco/embedding_lamlopt/P00hg/sum/daq",
                     "/star/rcf/data07/reco/embedding_pbar/P00hg/sum/daq",
);   
my @set ;
my @list;
my $nlist = 0; 
my @list_sum;     
my $job_log;
my $dummy;
my $num_event = 0;
my $EvSkip = 0;
my $EvDone = 0;
my $file_sum;
my $dir_lg       = "../log";
my $name_log;
my $f_flag = 0;
my $size;
my $mTime;
my $flname;
my $mfile;
my $msize;
my $fullname;

#=========================================================
my $ii =0;

struct FileAttr => {
       lfile    => '$',
       lsize    => '$',

 };

for ($ii = 0; $ii < scalar(@dir_log); $ii++)  {
  opendir(DIR, $dir_log[$ii]) or die "can't open $dir_log[$ii]\n";
  $nlist = 0;
  while( defined($flname = readdir(DIR)) ) {
           next if $flname =~ /^\.\.?$/;
           next if $flname =~ /.err/; 

        $fullname = $dir_log[$ii] ."/".$flname;
         $size = (stat($fullname))[7];
               
       $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->lfile($flname);
      ($$fObjAdr)->lsize($size);
       
       $list[$nlist] = $fObjAdr;
       $nlist++;
  }
 closedir DIR;


chdir $dir_log[$ii];

foreach my $logFile (@list) {
     
         $mfile = ($$logFile)->lfile;
         $msize = ($$logFile)->lsize;
       
        my $ltime = `mod_time $mfile`;
           if( $ltime > 7200){
		    if ($msize < 5000 )  {
#     print "Crashed job :", $mfile, "\n";
   }else { 
              $f_flag = 0;
              $file_sum = $mfile;
              $file_sum =~ s/.log//g;
              $file_sum = $file_sum . ".sum";
        chdir $dir_sum[$ii];

        if(-f $file_sum ) {
             my $stime = `mod_time $file_sum`;
        if( $ltime < $stime) {
#     print "Name of file: ", $file_sum, " logfile: ", $ltime, " sumfile: ", $stime, "\n";      
              $f_flag = 0;
         } else {
               $f_flag = 1;
	     }
           }
         chdir $dir_log[$ii];
              if($f_flag != 1) {  
#         print "Name of file: ", $mfile, "\n";
             parse_log($mfile);
             timestamp($mfile);
            $name_log = $mfile; 
         $dummy = `mv $file_sum $dir_sum[$ii]`;
#         $dummy = `mv $name_log $dir_lg`;   
           }             
        }
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

  my $input_fn;
  my $lib_version;  
  my $record_run_options = 0;
  my $run_option_length = 0;
  my $run_option_string;
  my $start_line = "";
     $num_event = 0;
  my $line; 
  my $jrun = "Run not completed";
  my $segmentation_violation;
  my $break_buss;
  my $Err_messg = "none";
  my $exmessg = "none";
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
  my @word_tr;
  my $i;
  my $noEvts = 0;
     $EvSkip = 0;
     $EvDone = 0;
  my $first_evts = 0;
  my $last_evts = 0; 
  my @nparts;
  my $last_maker = 0;
  my @size_line;
  my $ij = 0;
  my @line_tag;
  my $tag_flag = 0;
  my $tag_flag1 = 0;
  my $tag_flag2 = 0;  
  my @tag_word;
  my $last_tag_line;
  my $last_tag = "last";
  my $node_name = "n/\a";
  my $first_line;
  my $star_level = "n/\a";
  my $root_level = "n/\a"; 
   my $Anflag = 0;
#---------------------------------------------------------
  
# parse beginning of file
my $myTag = 0; 
my $chFlag = 0;
 
  $first_line = $logfile[1];
   chop $first_line ;
  foreach $line (@logfile) {
     chop $line ;
      $num_line++;
# get STAR_LEVEL and ROOT_LEVEL

     
    if ( $line =~ /QAInfo:You are using STAR_LEVEL/) {
       @nparts = split (" ", $line);
      $star_level = $nparts[5];
      $root_level = $nparts[8];    
     }
    
# get library version
    if ( (! $lib_version) && $line =~ /={3} You are in (\w+)/ ) {
      $lib_version = $1;
      $record_run_options = 1;
   $myTag++; 
   }
   
# get input file name
    if ( (! $input_fn) && $line =~ /Input file name = (\w+)/ ) {
      $input_fn = $1;
    }

     if ( $line =~ /QAInfo: Requested chain/ ) {
     $chFlag = 1;
  }
  if ( $line =~ /QAInfo: Input file name/ && $line =~ /QAInfo: Output root file name/) {
     $chFlag = 0;      
 }     


     if ( $chFlag == 1 ) {
   $run_option_string .= $line."\n";
  if ( $line =~ /QAInfo: Input file name/) {
     $chFlag = 0;
    $run_option_string .= ">>>>>>>>>>>>  Run options: <<<<<<<<<<<< "."\n";
  }
  elsif ( $line =~ /QAInfo: Output root file name/) {
   $chFlag = 0;   
 }
 }

# print tag for each Maker
     if( $myTag == 1) {

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
#}

# get memory size for each Maker

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
#   }
# get  number of events
    if ( $line =~ /QAInfo: Done with Event/ ) {
      @nparts = split ( "/",$line);
      $noEvts = $nparts[2];
      $last_evts = substr($noEvts,4) + 0; 
      if ($no_event == 0) {
      $first_evts = $last_evts;
    }
      $no_event++;
  } 

#}   
# check if job crashed due to break_buss_error
     if($line =~ /bus error/) {
         $Err_messg = "Break bus error";
       }


# check if job crashed due to segmentation violation
    if ($line =~ /segmentation violation/) {
             $Err_messg = "segmentation violation";
    }

     if ($line =~ /Interpreter error recovered/)  {
             $Err_messg = "Interpreter error recovered";
	   }

    $previous_line = $line;

# check how many events have been skiped

      if ( $line =~ /Total events processed/) {

        @word_tr = split /:/,$line;
        $EvSkip = $word_tr[3];
   }      
    
    
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
}
##----------------------------------------------------------------------------
# parse error log file

   my @err_out;
   my $mline;
   my $jLog_err;
 
    $jLog_err = $filename;
    $jLog_err =~ s/log/err/g;

     @err_out = `tail -100 $jLog_err`;
  foreach $mline (@err_out){
          chop $mline;
       if ( $mline =~ /No space left on device/)  {
        $exmessg = "No space left on device";
     } 
	elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $exmessg = $mline;
      }
     elsif ($mline =~ /Stale NFS file handle/) {
  
      $exmessg = "Stale NFS file handle";
 }       
       elsif ( $mline =~ /Assertion/ & $mline =~ /failed/)  {
        $Err_messg = "Assertion failed";
     } 
      elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $exmessg = $mline;
   }
      elsif ($mline =~ /Fatal in <operator delete>/) {
  
       $Err_messg = "Fatal in <operator delete>";   
  }
       elsif ($mline =~ /Fatal in <operator new>/) {
  
       $Err_messg = "Fatal in <operator new>";   
  }
      elsif ($mline =~ /Error: Unexpected EOF/) {
      $Err_messg = "Unexpected EOF";
  } 

  }
  
      if ( $Err_messg ne "none") {
     $jrun = $Err_messg;
   } 

#--------------------------------------------------------------------------
   $EvDone = $no_event;
 
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
  
 
    print ("Number of Events Done: ", $EvDone, "\n");
    print ("Number of Events Skiped: ", $EvSkip, "\n"); 
    print ("First event no.: ", $first_evts, "\n");
    print ("Last event no.: ", $last_evts, "\n");

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

  $num_event = $no_event - $EvSkip;


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
 @cpu_output = `tail -1000 $job_log`;
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
