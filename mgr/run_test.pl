#!/usr/star/bin/perl -w
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
my @input_dir = ("/star/rcf/disk0/star/test/venus412/b0_3",
                "/star/rcf/disk0/star/test/auau200/hadronic_cocktail/highdensity/year_1b/hadronic_on/Gstardata/",
                "/star/rcf/disk0/star/test/auau200/hadronic_cocktail/lowdensity/year_1b/hadronic_on/Gstardata/",
                "/star/rcf/disk0/star/test/auau200/hadronic_cocktail/standard/year_1b/hadronic_on/Gstardata/",
                "/star/rcf/disk0/star/test/auau200/hadronic_cocktail/highdensity/year_2a/hadronic_on/Gstardata/",
                "/star/rcf/disk0/star/test/auau200/hadronic_cocktail/lowdensity/year_2a/hadronic_on/Gstardata/",
                "/star/rcf/disk0/star/test/auau200/hadronic_cocktail/standard/year_2a/hadronic_on/Gstardata/");
my @hadronic = ("hc_highdensity", "hc_lowdensity", "hc_standard");
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
my @input_file = ("set0352_01_35evts.fzd","psc0208_01_40evts.fzd", "gstar.fzd"); 
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
   $input_ygeo[$i] = $input_dir[0] . $dir_year[$i] . $input_file[$i];
   $log_name = $input_file[$i];
   $log_name =~ s/.fzd//g;
   $log_file[$i] = $log_name . "." . "log";
}
#Set input and log file names for hadronic_coctail
   $log_file[2] = "gstar.log";

my $jj = 2;
for ($i= 1; $i < 7; $i++) {
    $input_ygeo[$jj] = $input_dir[$i] . $input_file[2];
    $jj++;
}   
  
# Set names of output directories
#
   my $ii = 0;
for ($i = 0; $i < 2; $i++) {
  for ($j = 0; $j < 2; $j++) {
 
   $output_dir[$ii] = $TOP_DIR . $node_dir[$i] . $thisday . $dir_year[$j];
        $ii++;
      }
}
   $jj = 4;
  for ( $i = 0; $i < 2; $i++) {
   for ($j = 0; $j < 3; $j++) {
  $output_dir[$jj] = $output_dir[$i] . $hadronic[$j] ;
    $jj++;
   }
 }    
  
# submitt job on tfs_Linux/.../year_1b

   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[0],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[0]);

  print ("Chain: ", $cmd_str, "\n");
  $cmd = "ssh rcas0212 \"$cmd_str >& $log_file[0] \&\"";
   system("$cmd &");

 $Nevent = 16;
   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[4],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[2]); 

   print ("Chain: ", $cmd_str, "\n"); 
   $cmd = "ssh rcas0213 \"$cmd_str >& $log_file[2] \&\"";

   system("$cmd &");
    
 $Nevent = 400;
   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[5],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[3]); 
 
   print ("Chain: ", $cmd_str, "\n");   
   $cmd = "ssh rcas0214 \"$cmd_str >& $log_file[2] \&\"";

   system("$cmd &");

 $Nevent = 40;
   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[6],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[4]); 
 
   print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rcas0216 \"$cmd_str >& $log_file[2] \&\"";

   system("$cmd &");

#submitt job on tfs_Linux/.../year_2a

$Nevent = 10;
    $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[1],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[1]);

 print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rcas0206 \"$cmd_str >& $log_file[1] \&\"";

   system("$cmd &");
 
 $Nevent = 16;
   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[7],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[5]); 
 
  print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rcas0207 \"$cmd_str >& $log_file[2] \&\"";

   system("$cmd &");
    
 $Nevent = 400;
   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[8],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[6]); 
 
  print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rcas0208 \"$cmd_str >& $log_file[2] \&\"";

   system("$cmd &");

 $Nevent = 40;
   $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[9],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[7]); 
 
 print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rcas0209 \"$cmd_str >& $log_file[2] \&\"";

   system("$cmd &");


#submitt job on rmine02 tfs_Solaris/.../year_1b
$Nevent = 10;

    $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[2],$Nevent,$Process[0],$gyear[0],$eval,$input_fm,$xdf_out,$input_ygeo[0]);

  print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rmine02 \"$cmd_str >& $log_file[0] \&\"";

   system ("$cmd &");

#submitt job on rmine02 tfs_Solaris/.../year_2a 

    $cmd_str = sprintf ("cd %s; root4star -b -q 'bfc.C(%d, \\\"%s %s %s %s %s\\\",\\\"%s\\\")'",$output_dir[3],$Nevent,$Process[0],$gyear[1],$eval,$input_fm,$xdf_out,$input_ygeo[1]);

 print ("Chain: ", $cmd_str, "\n");
   $cmd = "ssh rmine02 \"$cmd_str >& $log_file[1] \&\"";

   system ("$cmd &");


   system 'echo "Finished job execution at" `date` " on " `hostname`';   
  
  exit(0);
#============================================================================
#END
