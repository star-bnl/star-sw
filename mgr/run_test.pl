#!/usr/local/bin/perl -w
#
#use strict;
#use English;
use Sys::Hostname;
#my $hostname     = hostname();
# Account to send mail messages to on actions.  Should not be account this
#  script runs in (NOTE: the at sign (@) must be escaped as \@

my $EMAIL = "starreco\@bnl.gov";
#-----------------------------------------------------------------------

my $TOP_DIR = "/star/rcf/disk00000/star/test/dev/";
my @Process = ("tfs", "trs", "tss");
my $Nevent = 10;
my @gyear = ("y1b", "y2a");
my @dir_year = ("/year_1b/", "/year_2a/");
my $input_dir = "/star/rcf/disk0/star/test/venus412/b0_3";
my $sec;
my $min;
my $hour;
my $mday;
my $mon;
my $year;
my $wday;
my $yday;
my $isdst;
my $thisday;
my $thistime;
my @node_dir = ("tfs_Linux/", "tfs_Solaris/", "trs_Linux/", "trs_Solaris/");
my @input_file = ("set0352_01_35evts.fzd","psc0208_01_40evts.fzd"); 
my @log_file;
my $log_name;
my @input_ygeo;
my @output_dir;
my $cmd;
my $cmd_str;
my $eval = "eval";
my $xdf_out = "xout";
my $input_fm = "fzin";
my $i = 0;
my $j = 0;

#
# Set name of week day 
#
  ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    $thisday = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];
    $thistime = $hour . "." . $min . "." . $sec;
    print ("time is : ",  $thistime, "\n"); 

# Set input and log file names

for ($i = 0; $i < 2; $i++) {
   $input_ygeo[$i] = $input_dir . $dir_year[$i] . $input_file[$i];
   $log_name = $input_file[$i];
   $log_name =~ s/.fzd//g;
   $log_file[$i] = $log_name . "." . "log";
   print ("log file: ", $log_file[$i], "\n");
   print ("input file:", $input_ygeo[$i], "\n");
}
#
# Set names of output directories
#
   my $ii = 0;
for ($i = 0; $i < 2; $i++) {
  for ($j = 0; $j < 2; $j++) {
 
   $output_dir[$ii] = $TOP_DIR . $node_dir[$i] . $thisday . $dir_year[$j];
     print ("Output directories:", $output_dir[$ii], "\n");
        $ii++;
      }
}
# submitt job on tfs_Linux/.../year_1b

   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[0],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[0]);

   $cmd = "ssh rcas0212 \"$cmd_str >& $log_file[0] \&\"";

   system("$cmd &");

#submitt job on tfs_Linux/.../year_2a

    $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[1],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[1]);

   $cmd = "ssh rcas0213 \"$cmd_str >& $log_file[1] \&\"";

   system("$cmd &");
 
#submitt job on rsun00 tfs_Solaris/.../year_1b

    $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[2],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[0]);

   $cmd = "ssh rsun00 \"$cmd_str >& $log_file[0] \&\"";

   system ("$cmd &");

#submitt job on rmds03 tfs_Solaris/.../year_2a 

    $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[3],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[1]);

   $cmd = "ssh rmds03 \"$cmd_str >& $log_file[1] \&\"";

   system ("$cmd &");


   system 'echo "Finished job execution at" `date` " on " `hostname`';   
  
  exit(0);
#============================================================================
#END
