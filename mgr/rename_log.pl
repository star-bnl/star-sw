#!/opt/star/bin/perl -w

# script to rename log and err files
#================================================================
use strict;
my $log_file;
my $dir_log = "/disk00001/star/MDC3/test";
my $err_file;
my $file_name;
my $erfile_name;
my $dummy;
my @words;
my @log_list;
my @err_list;
#================================================================
chdir $dir_log;
@log_list = `ls *evts`;
foreach $log_file (@log_list){
     chop $log_file;
     @words = split("_",$log_file);
      $file_name = $words[9] . "_" . $words[10] . "_" . $words[11] . ".log";  
#       print ($file_name, "\n");
       $dummy = `mv $log_file $file_name`;
   }

@err_list =`ls *evts.err`;  
foreach $err_file (@err_list){
      chop $err_file;
     @words = split("_",$err_file);
      $erfile_name = $words[9] . "_" . $words[10] . "_" . $words[11];
#      print ($erfile_name, "\n");
      $dummy = `mv $err_file $erfile_name`;
   }
exit(0);
