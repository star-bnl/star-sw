#! /usr/local/bin/perl
#
# Script to print production statistics and produce job script files
#
use strict;
use Sys::Hostname;
#use FileHandle;
use English;
my $caller_name  = 'catalog.pl';
my @PROCESSES    = ('tfs_dst');
my $process;
my $DISK1        = "/disk1";
my $HOSTNAME     = hostname();
my $RCF          = "rcf.rhic.bnl.gov";
#if ($HOSTNAME == $RCF) {$DISK1 = "/net/rmds03/disk1";}
my $TOPHPSS_SINK =  "/home/starsink/raw";
my $TOPHPSS_RECO =  "/home/starreco/reco";
my $TOPDISK1_RECO=  $DISK1 . "/star";
my $TOP_TEST     =  "/star/scr2f/starreco/MDC1/tests";
my $REQUEST      =  "/star/u2e/starreco/MDC1/requests";
my $JOB_SUMMARY  =  "/star/u2e/starreco/MDC1/summary";
my $JOB_LOG      =  $DISK1 . "/star/MDC1";
my $Objy         =  $DISK1 . "/star/stardb/dst";
my @SETS = @ARGV;
#my @SETS = ("b0_3/year2a/hadronic_on");

my @MORE_SETS=(
            "b0_20/year2a/hadronic_on",
            "b0_20/year2x/hadronic_on",
            "b0_20/year_1b/hadronic_off",
            "b0_2/year1a/hadronic_off",
            "b0_2/year1a/hadronic_on",
            "b0_2/year2a/hadronic_off",
            "b0_2/year2a/hadronic_on",
            "b0_2/year2y/hadronic_on",
            "b0_3/year1a/hadronic_off",
            "b0_3/year1a/hadronic_on",
            "b0_3/year2a/hadronic_on",
            "b0_3/year2x/hadronic_on",
            "b0_3/year_1b/hadronic_off");
my %geant_size = ();
my %geant_date = ();
my %geant_test = ();         # test of input (Y/N)
my %geant_noev = ();
my %reco_size = ();
my %reco_date = ();
my %dst_size = ();           # dst on disk1
my %dst_date = ();
my %Objy_size = ();          # dst in Objy
my %Objy_date = ();
my %files = ();
my %files_job = ();
my %comment = ();
my %jobfile = ();
my %archive = ();
my %running = ();
my %processed = ();
my %noevents = ();
my %timeperevent = ();
my %kBsec = ();
my %node = ();
my %job_status = ();
my @chars;
#___________________________________________
my $set;
my $no_events;
my $input;
my $input_files;
my $output;
my $output_files;
my $gen_set;
my $raw_set;                  # list of Gstar files on HPSS
my $reco_set;                 # list of tfs_dst files on HPSS
my $answer;                   # output of system calls
my $catalog;
my %list_of_files;
my $dummy;
my $day;
my $time;
my $time2;
my $filename;
my @lines;
my @words;
my $line;
my $file;
my $dir;                       # current directory
my $tag;                       #
my @out;
my $t;
my $gsze;
my $gdat;
my $GJB;
my $dsze;
my $ddat;
my $run;
my $nev;
my $tpe;
my $last_line;
#_________________ parameters for stat
my $dev;  # device number of filesystem
my $ino;  # Inode number
my $mode; # File mode (type and permissions)
my $nlink;# Number of (hard) links to the file
my $uid;  # Numeric user ID of the files's owner
my $gid;  # Numeric group ID of the files's owner
my $rdev; # The device identifier (special files only)
my $size; # Total size of the file, in bytes
my $atime;# Last access time since the epoch
my $mtime;# Last modify time since the epoch
my $ctime;# Inode change time (NOT creation time!) since the epoch
my $blksize;# Preferred blocksize for the file system I/O
my $blocks;# Actual number of blocks allocated
my $now;  # current time
my $full_name;
my $dtime;
$now = time;
#_________________ parameters for localtime
my $sec;
my $min;
my $hour;
my $mday;
my $mon;
my $year;
my $wday;
my $yday;
my $isdst;
my $month;
my $BJOBS;
my $CRS_JOBS;
# Subroutines
sub usage
  {
    printf("\nUsage: %s <job id>_<subjob number> | job_<job id>_<subjob number>\n",$0);
    printf("\tWhere <job id> is the job identifier of the job to kill and\n");
    printf("\t<subjob number> is the subjob number of the job to kill.\n");
    printf("\tThe second form with the string \"job_\" prepended is the form\n");
    printf("\tpresented by the \"crs_node_status.pl\" script.\n");
    exit(1);
  }

#------------------------------------------
#format STDOUT_TOP =
#set = b0_3/year_1b/hadronic_off Input Total = 249.671 (GB) in 256 files -> Output Total = 0(GB) in 0 files
#set = @<<<<<<<<<<<<<<<<<<<<<<<< Input Total = @<<<<<< (GB) in @<< files -> Output Total = @>> (GB) in @>> files
#      $set,                                   $input,         $input_files,               $output,    $output_files
#.
format STDOUT_TOP =
set =@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$set,
                   size            dst on HPSS   on disk1   in Objy                  No. time   Rate  Comments
         Filename  (MB)   date GJA (MB)   date (MB)   date (MB)   date  run          ev.(mins) (kB/s) cpu\tot (hrs)
.
format STDOUT =
@<<<<<<<<<<<<<<<<<@>>>> @>>>>>@|||||@>>@>>>>>>@>>>>@>>>>>>@>>>>@>>>>>> @<<<<<<<<<<<<@>>> @>>>> @<<<< @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$file,$geant_size{$file},$geant_date{$file},$GJB,$reco_size{$file},$reco_date{$file},$dst_size{$file},$dst_date{$file},$Objy_size{$file},$Objy_date{$file},$running{$file},$noevents{$file},$timeperevent{$file},$kBsec{$file},$comment{$file}
.
#  $~ = "STDOUT";
#$BJOBS = `bjobs -u starreco`;
$CRS_JOBS = `crs_status.pl`; #print $CRS_JOBS;
@lines = split /^/m, $CRS_JOBS;
my $job_status;
foreach $line (@lines){
   @words = split (" ",$line);#   print $line, $#words, "\n";
   next if $#words<2;
   if (grep (/Running|Queuing|Cleanin|Staging|Trans|Not/,$words[2])) {# 907967368       0        Queuing
     $job_status = $words[2];#     print $job_status;
   }
   else {
     if ($#words ==3) {# Job Description file: b0_20y2a_on_psc256_03_200evts
       my $file = $words[$#words];
       $file =~ s/b0_20y2a_on_|b0_20y2x_on_|b0_20y_1b_off_|b0_2y1a_off_|b0_2y1a_on_//g;
       $file =~ s/b0_2y2a_off_|b0_2y2a_on_|b0_2y2y_on_|b0_3y1a_off_|b0_3y1a_on_//g;
       $file =~ s/b0_3y2a_on_|b0_3y2x_on_|b0_3y_1b_off_|//g;
       $job_status{$file} = $job_status;
       $files_job{$file} = $file;
    }
   }
}
my $NODE_JOBS = `crs_node_status.pl`;
@lines = split /^/m, $NODE_JOBS;
my $no_of_jobs;
my $rnode;
foreach $line (@lines){
   @words = split (" ",$line); #   printf ("%s %d \n",$line,$#words);
   next if $#words <0;
   next if grep (/Node/,$words[0]);
   next if grep (/Status/,$words[0]);
   if (grep (/rcrs/,$words[0])) {
     ($rnode) = split /\./, $words[0];
     $no_of_jobs = $words[1]; #     printf ("%s %d\n",$rnode,$no_of_jobs);
   }
   if (grep (/Job/,$words[0])) {
     my $i;
     for ($i=1;$i<=$no_of_jobs;$i++){
       my $j = $i + 2;
       $file = $words[$j]; # printf ("file = %d %s %s\n",$j,$words[$j],$file);
       $file =~ s/b0_20y2a_on_|b0_20y2x_on_|b0_20y_1b_off_|b0_2y1a_off_|b0_2y1a_on_//g;
       $file =~ s/b0_2y2a_off_|b0_2y2a_on_|b0_2y2y_on_|b0_3y1a_off_|b0_3y1a_on_//g;
       $file =~ s/b0_3y2a_on_|b0_3y2x_on_|b0_3y_1b_off_|//g;
       $node{$file} = $rnode; #       printf ("%s %s\n",$file,$rnode);
     }
   }
}
#foreach  $file (sort keys %files_job){printf ("%s %s %s \n",$file,$job_status{$file},$node{$file});}
foreach $set (sort @SETS){
#
  printf ("set = %s \n", $set);
  $gen_set = $set;
  $gen_set =~ s/auau200/auau_/g;
  $gen_set =~ s/hijing135/hijing/g;
  $gen_set =~ s/default//g;
  $gen_set =~ s/complete/com/g;
  $gen_set =~ s/ear//g;
  $gen_set =~ s/hadronic//g;
  $gen_set =~ s|/||g;
  $raw_set = $gen_set . ".list";
  printf ("gen_set = %s raw_set = %s \n", $gen_set, $raw_set);
#  if (! -e $raw_set) {
    print ("raw_set does not exist \n");
    print ("%s %s \n", $TOPHPSS_SINK,  $set);
    $answer = `ftp -i -v rmds01 2121 <<EOF
cd $TOPHPSS_SINK
cd $set/gstardata
mdir *.fzd $raw_set
EOF
`;
#  }
  foreach $process (@PROCESSES){
#    printf ("process = %s",$process);
    my $jb = $gen_set . ".err";
    my $jb_old = $jb . ".old";
    if (-f $jb) {my $mv = `mv $jb $jb_old`;}
    if (open(ERR_LOG,">$jb")) {
#      printf ("Create log file  %s\n",$jb);
    }
    else {
      printf ("Unable to create log file\n");
      next;
    }
     $reco_set = $process . $gen_set . ".list";
    $catalog =  $process . $gen_set . ".catalog";
    if (! -e $reco_set) {
      $answer = `ftp -i rmds01 2121 <<EOF
cd ${TOPHPSS_RECO}
cd ${set}/$process
mdir *.xdf ${reco_set}
EOF
`;
    }
  }
# raw data set
  open(RAW_SET,"$raw_set" ) || die "Can't open RAW_SET $raw_set: $!\n";
  while ($line=<RAW_SET>){
#       printf ("%s\n",$line);
      ($dummy, $dummy, $dummy, $dummy, $size, $mon, $day, $time, $filename ) = split (" ",$line);
#       printf ("filename = %s date =  %s %s %s size = %s\n", $filename,$mon, $day, $time, $size );
      $file = $filename;
      $file =~ s/.fzd//g;
#      printf  ("file = %s size = %s\n", $file, $size);
      $files{$file}       = $file;
      $size = $size/1000000;
      $geant_size{$file} .= int $size;
      $geant_date{$file} .= $mon  . "_" . $day ;
      $geant_test{$file}  = "N";
      $reco_size{$file}   = "";
      $reco_date{$file}   = "";
      $dst_size{$file}    = "";
      $dst_date{$file}    = "";
      $Objy_size{$file}   = "";
      $Objy_date{$file}   = "";
      $kBsec{$file}       = "";
      $comment{$file}     = "";
      $jobfile{$file}     = "N";
      $archive{$file}     = "N";
      $running{$file}     = "";
  }
  close(RAW_SET);
# reco data set
  foreach $process (@PROCESSES){
    $reco_set = $process . $gen_set . ".list";
    open(RECO_SET,"$reco_set") || die "Can't open RECO_SET $reco_set: $!\n";
    while ($line=<RECO_SET>){
      ($dummy, $dummy, $dummy, $dummy, $size, $mon, $day, $time, $filename ) = split (" ",$line);
#       printf ("filename = %s date =  %s %s %s size = %s\n", $filename,$mon, $day, $time, $size );
      $file = $filename;
      my @h_dst = grep /_h_/, $filename;
      if ($#h_dst &&  $reco_date{$file}) {next;} # skip dst if _h_ already exists
      $file =~ s/(_|_h_)dst.xdf//g;
#      printf  ("file = %s size = %s\n", $file, $size);
      $files{$file}       = $file;
      $size = $size/1000000;
      $reco_size{$file} = int $size;
      if ($geant_size{$file}) {my $ratio = $reco_size{$file}/$geant_size{$file};
          if ($ratio < 0.1) {$reco_size{$file} = "";}
#          printf ("reco/geant ratio = %f\n",$ratio);}
      }
      if ($reco_size{$file} == 0) {$reco_size{$file} = "";}
      $reco_date{$file} = $mon  . "_" . $day;
      if (! defined $geant_date{$file}) { $geant_date{$file} = "";}
      if (! defined $geant_size{$file}) { $geant_size{$file} = "";}
      if (! defined $geant_test{$file}) { $geant_test{$file} = "N";}
      if (! defined $jobfile{$file})    { $jobfile{$file} = "N";}
      if (! defined $archive{$file})    { $archive{$file} = "N";}
    }
    close(RECO_SET);
  }
#  my $count = 0;
#  foreach  $file (sort keys %files){
#     $count++;
#     printf ("file = %s no %d\n",$file, $count);
#  }

#_____________________ scalars for summary
my $geant_size = 0;
my $reco_size = 0;
my $dst_size = 0;
my $dst_size_tot = 0;
my $Objy_size = 0;
my $noevents = 0;
my $no_ev_tot = 0;
my $no_ev;
  foreach  $file (sort keys %files){
# /disk1/dst
    ($dummy, $dummy, $no_ev) = split /_/, $file;
    $no_ev =~ s/evts//g;# printf ("file = %s noev = %d\n", $file, $no_ev);
    if ($no_ev) {$no_ev_tot += $no_ev;}
    if ($no_ev && $kBsec{$file}) {
       $kBsec{$file} = $kBsec{$file} * $noevents{$file} / $no_ev;
       if (! $reco_size{$file} && $dst_size{$file}) {$kBsec{$file} = "";}
    }
    $dir = $TOPDISK1_RECO . "/" . $set . "/tfs_dst/";
    $full_name = $dir . $file . "_h_dst.xdf";# printf ("full name = %s\n",$full_name);
    if (! -f $full_name) {$full_name = $dir . $file . "_dst.xdf";}
    if (-f $full_name){
      $line = `ls -l  $full_name`;# printf ("line = %s\n", $line);
      ($dummy, $dummy, $dummy, $dummy, $size, $mon, $day, $time, $filename ) = split (" ",$line);
#      printf ("file = %s size = %d date %s %s \n", $full_name, $size, $mon, $day);
      $size = $size/1000000;
      $dst_size{$file} = int $size;
      $dst_date{$file} = $mon . "_" . $day;
    }
# Objy files
    $dir = $Objy . "/";
    $full_name = $dir . $file . "_h_dst.xdf.STAR.DB";# printf ("full name = %s\n",$full_name);
    if (! -f $full_name) {$full_name = $dir . $file . "_h_dst.xdff.STAR.DB";}# printf ("full name = %s\n",$full_name);}
    if (! -f $full_name) {$full_name = $dir . $file . "_dst.xdf.STAR.DB";}# printf ("full name = %s\n",$full_name);}
    if (-f $full_name){
      $line = `ls -l  $full_name`;# printf ("line = %s\n", $line);
      ($dummy, $dummy, $dummy, $dummy, $size, $mon, $day, $time, $filename ) = split (" ",$line);
#      printf ("file = %s size = %d date %s %s \n", $full_name, $size, $mon, $day);
      $size = $size/1000000;
      $Objy_size{$file} = int $size;
      $Objy_date{$file} = $mon . "_" . $day;
     }

#  geant test logfiles
    $dir = $TOP_TEST . "/" . $set . "/Gstardata";
    ($tag) = split /_/, $file, 3;
    $tag .= ".log";
    @out = `find $dir -name $tag`;
    $noevents{$file} = "";
    $timeperevent{$file} = "";
#    printf ("file = %s dir = %s tag = %s out = %s\n", $file, $dir, $tag, @out);
    if ($out[0]) {$geant_test{$file}  = "Y";}
# log files comments
    $dir = $JOB_LOG . "/" . $set . "/tfs_dst";#    printf ("file = %s \n", $file);
#    my @outfind = `find $dir -name $tag -a ! -name $tag.err -a ! -name $tag.old`;# print @outfind;
    $tag = $dir . "/" . $file . "*";
    my @log = glob $tag;
    my @outfind;
    if ($log[0]) {@outfind = `ls -t $tag`;}# print @outfind;
    if ($outfind[0]) {
      my $count=0;
      foreach $t (@outfind){
        $count++;
        if ($count ==1){
          my @output = `tail -100 $t`;
          my @string = grep /total/, @output;
          if ($string[0]){
             my @newstring = grep /process/, @string;
             if ($newstring[0]){
               my @last_line = grep /\<bfc\>/, @newstring;#printf ("last = %s\n",$last_line[0]);
               $last_line = $last_line[0];
               if ($last_line){#            printf ("%s\n", $last_line);
                 ($dummy, $dummy, $dummy, $dummy, $dummy, $no_events, $dummy, $time2, $time) = split (" ",$last_line);
                 my $ev = $no_events;
                 $ev =~ s/events//g;
                 $time2 =~ s/=//g;
                 if ($time2) {$no_events = $ev;
                              $time = $time2;
                 }
                 $noevents{$file} = $no_events;
#                printf ("no_events %d time = %d\n",$no_events,$time);
                 $timeperevent{$file} = $time/60/$no_events;
                 $running{$file} = "Done";
               }
             }
          }
          @string = grep /pf+/, @output;# printf ("string %s\n", $string[0]);
          if ($string[0]) {
            my $cpu;
            my $tot;
            ($cpu, $dummy, $tot) = split / /,$string[0];
            $cpu =~ s/u//g;
            if ($cpu && $geant_size{$file} && $noevents{$file}) {
              $kBsec{$file} = 1000*$geant_size{$file}/$cpu;
#             printf ("cpu = %f size =%f MGB/sec = %f\n",  $geant_size{$file}, $cpu, $kBsec{$file});
            }
            $cpu = (int $cpu/36)/100;
            @string = split /\./, $tot;
            $tot = $string[0];
            $comment{$file} = $cpu . '\/' . $tot;
          }
        }
        $comment{$file} .= $t;
        $comment{$file} =~ s/$dir//g;
        $comment{$file} =~ s/$file//g;
        $comment{$file} =~ s/.err//g;
        $comment{$file} =~ s|/old/|old|g;
        $comment{$file} =~ s|/||g;
        $comment{$file} =~ s/.old/|/g;
        $comment{$file} =~ s/old/|/g;
        $comment{$file} =~ s/_/ /g;
        $comment{$file} =~ s/done//g;
        $comment{$file} =~ s/-Aft-//g;
#        printf ("comment = \%s",  $comment{$file});
      }
    }
# process
    foreach $process (@PROCESSES){
       my $jb;
       $jb =  $REQUEST . "/" . $process . "/jobfiles/" .  $gen_set . "_" . $file;
       if (-f $jb) {$jobfile{$file} = "Y";}
       $jb =  $REQUEST . "/" . $process . "/" . "/archive/" . $file;
       if (-f $jb) {$archive{$file} = "Y";} # printf ("archive %s \n",$archive{$file});}
#        $jb =  $REQUEST . "/" . $process . "/" . $process . ".lsf/" . $file;
#       if (-f $jb) {$archive{$file} = "Y";} # printf ("archive %s \n",$archive{$file});}
# summary file
      $jb = "";
      my $full_tag =   $JOB_SUMMARY . "/" . $process . "/" . $file . "*";
      my @jb = glob $full_tag;  #      printf ("found %d summary files %s\n",$#jb);
      if ($#jb < 0) {next;}
      if ($#jb>0) {
        print "Multiple summary files\n";
        for (my $i = 0; $i<=$#jb; $i++){
          printf ("summary file = %s\n",$jb[$i]);
        }
      }
        $jb = $jb[0];
        $running{$file} = "???";# printf ("%s %s\n",   $running{$file}, $jb);
        $processed{$file} = "No";
        $full_tag =   $JOB_SUMMARY . "/" . $process . "/" . $file . "*" . ".done";
        my @jb = glob $full_tag;
        $jb = "";
        if ($#jb > -1) {$jb = $jb[0];}
        my $rnode;
        if ($jb) {
          $running{$file} = "done";
          $last_line = `grep 'total\ CPU\ to\ process' $jb`;
          if ($last_line){#            printf ("%s\n", $last_line);
            ($dummy, $dummy, $dummy, $dummy, $dummy, $no_events, $dummy, $time2, $time) = split (" ",$last_line);
            my $ev = $no_events;
            $ev =~ s/events//g;
            $time2 =~ s/=//g;
            if ($time2) {
              $no_events = $ev;
              $time = $time2;
            }
            $noevents{$file} = $no_events;#            printf ("no_events %d time = %d\n",$no_events,$time);
            $timeperevent{$file} = $time/60/$no_events;
          }
          ($dummy, $rnode) = split '\.', $jb;# printf ("jb = %s rnode = %s\n", $jb, $rnode);
          $running{$file} = $running{$file} . "_" . $rnode;
        }
        if ($job_status{$file}) {
#          if (grep (/\?\?/, $running{$file}) ==0) { print $file "job conflict";}
          @chars = split //, $job_status{$file};
          $running{$file} = $chars[0] . $chars[1] . $chars[2] . $chars[3] . "_" . $node{$file};
        }
#     printf ("Run status %s from summary file %s\n", $running{$file},$jb);
    }
#    printf ("comment = %s", $comment{$file});
#    printf ("file = %s, geant_size = %s, date = %s reco_size = %s reco_date = %s",
#   $file, $geant_size{$file}, $geant_date{$file}, $reco_size{$file}, $reco_date{$file});
#   printf (" gtest = %s comment = %s\n", $geant_test{$file}, $comment{$file}) ;
     if (! $reco_size{$file} && !$dst_size{$file} && grep (/[Dd]one/,$running{$file})) {
       $running{$file} = "";
     }
     $GJB  = $geant_test{$file} . $jobfile{$file} . $archive{$file};
     write;
# summary
     if ($geant_size{$file})    {$geant_size    +=  $geant_size{$file};}
     if ($reco_size{$file})     {$reco_size     +=  $reco_size{$file};}
     if ($dst_size{$file})      {$dst_size      +=  $dst_size{$file};}
     if ($Objy_size{$file})     {$Objy_size     +=  $Objy_size{$file};}
     if ($noevents{$file})      {$noevents      +=  $noevents{$file};}
     if ($reco_size{$file})     {$dst_size_tot  +=  $reco_size{$file};}
     else {if ($dst_size{$file})    {$dst_size_tot  +=  $dst_size{$file};}}

  }
 printf (" ------- Summary for set = %s ----------------------\n",$set);
 $geant_size = $geant_size/1000;
 $reco_size  = $reco_size/1000;
 $dst_size   = $dst_size/1000;
 $Objy_size  = $Objy_size/1000;
 $dst_size_tot =  $dst_size_tot/1000;
 printf (" GEANT = %6.2f GB with %d events, dst = %6.2f (HPSS) + %6.2f (DISK1) = %6.2f GB with %d events, Objy = %6.2f GB\n", $geant_size, $no_ev_tot, $reco_size, $dst_size, $dst_size_tot, $noevents, $Objy_size);
  foreach $process (@PROCESSES){
#_______lsf
    my $jb = $REQUEST . "/" . $process . "/" . $process . ".lsf/" . $gen_set;
    my $jb_old = $jb . ".old";
    if (-f $jb) {my $mv = `mv $jb $jb_old`;}
#    if (open(JOB_SCRIPT,">$jb")) {
#      printf ("Create job script to submit jobs %s\n",$jb);
#      print JOB_SCRIPT "#! /usr/local/bin/tcsh -f\n";
#    }
#    else {
#      printf ("Unable to create job submission script\n");
#      next;
#    }
    my $jb = $gen_set . ".del";
    my $jb_old = $jb . ".old";
    if (-f $jb) {my $mv = `mv $jb $jb_old`;}
    if (open(DEL_SCRIPT,">$jb")) {
#      printf ("Create job script to delete dst files %s\n",$jb);
      print DEL_SCRIPT "#! /usr/local/bin/tcsh -f\n";
    }
    else {
      printf ("Unable to create file delete script\n");
      next;
    }
    my $jb = $gen_set . ".ftp";
    my $jb_old = $jb . ".old";
    if (-f $jb) {my $mv = `mv $jb $jb_old`;}
    if (open(FTP_SCRIPT,">$jb")) {
      my $ftp_log = $jb . ".log";
#      printf ("Create job script to ftp dst files %s\n",$jb);
      print FTP_SCRIPT "#! /usr/local/bin/tcsh -f\n";
      print FTP_SCRIPT "pftp rmds01 2121 <<EOF | tee -a $ftp_log\n";
      print FTP_SCRIPT "bin\n";
      print FTP_SCRIPT "quote site setcos 7\n";
    }
    else {
      printf ("Unable to create file ftp script\n");
      next;
    }
    foreach  $file (sort keys %files){
      my $input_file = $TOPHPSS_SINK . "/" . $set . "/gstardata/" . $file . ".fzd";
# No dst on HPSS, No dst on disk1 and job is not running
      if ($geant_size{$file} && ! $reco_size{$file} && ! $dst_size{$file}){
        if (! $running{$file}){
#          print   JOB_SCRIPT "lsf_sub.csh $input_file \n";
#_______Tom
    my $Tom = "";
    if ($Tom){
    my $jb_tom  = $REQUEST . "/" . $process . "/jobfiles/" .  $gen_set . "_" . $file;
    my $jb_arc  = $REQUEST . "/" . $process . "/archive/"  .  $gen_set . "_" . $file;
    my $jb_hold = $REQUEST . "/" . $process . "/hold_jobs/"  .  $gen_set . "_" . $file;
    if (! -f $jb_tom && ! -f $jb_arc && ! -f $jb_hold) {
      my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $set . "/gstardata";
      my $hpss_raw_file = $file . ".fzd";
      my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $set . "/" . $process;
      my $hpss_dst_file = $file . "_h_dst.xdf";
      my $executable    = $REQUEST . "/" . $process . "/" . $process;
      my $log_dir       = $JOB_LOG . "/" . $set . "/" . $process;
      my $err_log       = $file . ".err";
      if (!open (TOM_SCRIPT,">$jb_tom")) {printf ("Unable to create job submission script\n");}
      print TOM_SCRIPT "#\n";
      print TOM_SCRIPT "#  This is a sample Central Reconstruction Job Specification File\n";
      print TOM_SCRIPT "#\n";
      print TOM_SCRIPT " \n";
      print TOM_SCRIPT "mergefactor=1\n";
      print TOM_SCRIPT "#input\n";
      print TOM_SCRIPT "      inputnumstreams=1\n";
      print TOM_SCRIPT "      inputstreamtype[0]= HPSS\n";
      print TOM_SCRIPT "      inputdir[0]=$hpss_raw_dir\n";
      print TOM_SCRIPT "      inputfile[0]=$hpss_raw_file\n";
      print TOM_SCRIPT " \n";
      print TOM_SCRIPT "#output\n";
      print TOM_SCRIPT "      outputnumstreams=1\n";
      print TOM_SCRIPT "#output stream \n";
      print TOM_SCRIPT "      outputstreamtype[0]= HPSS\n";
      print TOM_SCRIPT "      outputdir[0]=$hpss_dst_dir\n";
      print TOM_SCRIPT "      outputfile[0]=$hpss_dst_file\n";
      print TOM_SCRIPT " \n";
      print TOM_SCRIPT "#standard out -- Should be five outputs\n";
      print TOM_SCRIPT "      stdoutdir=$log_dir\n";
      print TOM_SCRIPT "      stdout=$file\n";
      print TOM_SCRIPT " \n";
      print TOM_SCRIPT "#standard error -- Should be five\n";
      print TOM_SCRIPT "      stderrdir=$log_dir\n";
      print TOM_SCRIPT "      stderr=$err_log\n";
      print TOM_SCRIPT " \n";
      print TOM_SCRIPT "      notify=starreco\@rcf.rhic.bnl.gov\n";
      print TOM_SCRIPT " \n";
      print TOM_SCRIPT "#program to run\n";
      print TOM_SCRIPT "      executable=$executable\n";
      print TOM_SCRIPT " \n";
      close(TOM_SCRIPT);
    }
#          print   JOB_SCRIPT "lsf_sub.csh $input_file \n";
#_______root
    my $root = "Yes";
    if ($root){
    my $jb_root = $REQUEST . "/" . $process . "/jobfiles/" .  $gen_set . "_" . $file;
    my $jb_arc  = $REQUEST . "/" . $process . "/archive/"  .  $gen_set . "_" . $file;
    my $jb_hold = $REQUEST . "/" . $process . "/hold_jobs/"  .  $gen_set . "_" . $file;
    if (! -f $jb_root && ! -f $jb_arc && ! -f $jb_hold) {
      my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $set . "/gstardata";
      my $hpss_raw_file = $file . ".fzd";
      my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $set . "/" . $process;
      my $hpss_root_file = $file . ".root";
      my $hpss_dst_file = $file . "_dst.xdf";
      my $executable    = $REQUEST . "/" . $process . "/" . $process . ".csh";
      my $log_dir       = $JOB_LOG . "/" . $set . "/" . $process;
      my $err_log       = $file . ".err";
      if (!open (root_SCRIPT,">$jb_root")) {printf ("Unable to create job submission script\n");}
      print root_SCRIPT "#\n";
      print root_SCRIPT "#  This is a sample Central Reconstruction Job Specification File\n";
      print root_SCRIPT "#\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "mergefactor=1\n";
      print root_SCRIPT "#input\n";
      print root_SCRIPT "      inputnumstreams=1\n";
      print root_SCRIPT "      inputstreamtype[0]= HPSS\n";
      print root_SCRIPT "      inputdir[0]=$hpss_raw_dir\n";
      print root_SCRIPT "      inputfile[0]=$hpss_raw_file\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "#output\n";
      print root_SCRIPT "      outputnumstreams=2\n";
      print root_SCRIPT "#output stream \n";
      print root_SCRIPT "      outputstreamtype[0]= HPSS\n";
      print root_SCRIPT "      outputdir[0]=$hpss_dst_dir\n";
      print root_SCRIPT "      outputfile[0]=$hpss_root_file\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "#output stream \n";
      print root_SCRIPT "      outputstreamtype[1]= HPSS\n";
      print root_SCRIPT "      outputdir[1]=$hpss_dst_dir\n";
      print root_SCRIPT "      outputfile[1]=$hpss_dst_file\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "#standard out -- Should be five outputs\n";
      print root_SCRIPT "      stdoutdir=$log_dir\n";
      print root_SCRIPT "      stdout=$file\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "#standard error -- Should be five\n";
      print root_SCRIPT "      stderrdir=$log_dir\n";
      print root_SCRIPT "      stderr=$err_log\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "      notify=starreco\@rcf.rhic.bnl.gov\n";
      print root_SCRIPT " \n";
      print root_SCRIPT "#program to run\n";
      print root_SCRIPT "      executable=$executable\n";
      print root_SCRIPT " \n";
      close(root_SCRIPT);
    }
}
        }
        else {print ERR_LOG  "file = $file run status is $running{$file}  but dst is missing \n" ;}
      }
      else {
        if ($reco_size{$file} && $dst_size{$file}){
          if ($reco_size{$file} == $dst_size{$file}){
            my $delete_file = $TOPDISK1_RECO . "/" . $set . "/" .  $process . "/" . $file . "_h_dst.xdf";
            print  DEL_SCRIPT "rm $delete_file\n";
          }
        }
      }
      if ($dst_size{$file} && ! $reco_size{$file} || $dst_size{$file} > ! $reco_size{$file}){
        my $disk1_dir = $TOPDISK1_RECO . "/" . $set . "/" .  $process;
        my $hpss_dir  = $TOPHPSS_RECO . "/" . $set . "/" .  $process;
        my $ftp_file  = $file . "_h_dst.xdf";
        print FTP_SCRIPT "lcd $disk1_dir\n";
        print FTP_SCRIPT "cd  $hpss_dir\n";
        print FTP_SCRIPT "pput  $ftp_file\n";
      }
    }
#    close (JOB_SCRIPT);
    close (DEL_SCRIPT);
    print FTP_SCRIPT "dir\n";
    print FTP_SCRIPT "EOF\n";
    close (FTP_SCRIPT);
    close (ERR_LOG);
  }
}
exit(0);
# last line
