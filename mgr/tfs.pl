#!/usr/local/bin/perl
use Env;
use File::Basename;
system 'echo "Starting job execution at" `date` " on " $HOSTNAME';
print "STAR_LEVEL = $STAR_LEVEL\n";
my  $input_file;
if ($ACTUAL_INPUT0) {
  chomp ($ACTUAL_INPUT0);
  print "Input: ",$ACTUAL_INPUT0, "\n";
  $input_file = basename($ACTUAL_INPUT0);
  print "input: ",$input_file,"\n";
  my $OUTPUT_FILE0 = convert($ACTUAL_OUTPUT0);
  my $OUTPUT_FILE1 = convert($ACTUAL_OUTPUT1);
  my $OUTPUT_FILE2 = convert($ACTUAL_OUTPUT2);
  my $OUTPUT_FILE3 = convert($ACTUAL_OUTPUT3);
  print "Output:", $ACTUAL_OUTPUT0,",",$ACTUAL_OUTPUT1,",",$ACTUAL_OUTPUT2,",",$ACTUAL_OUTPUT3,"\n";
  print "Output:", $OUTPUT_FILE0,",",$OUTPUT_FILE1,",",$OUTPUT_FILE2,",",$OUTPUT_FILE3,"\n";
  my $output_file0 = basename($OUTPUT_FILE0);
  my $output_file1 = basename($OUTPUT_FILE1);
  my $output_file2 = basename($OUTPUT_FILE2);
  my $output_file3 = basename($OUTPUT_FILE3);
  my $out_dir      = dirname ($OUTPUT_FILE0);
  print "Output:", $output_file0,",",$output_file1,",",$output_file2,",",$output_file3,"\n";
  print "Dirname:", $out_dir, "\n";
  if ($out_dir && !-d $out_dir) {mkdir $out_dir,0773;}
  if (-f $output_file0) {system 'rm $output_file0';}
  if (-f $output_file1) {system 'rm $output_file1';}
  if (-f $output_file2) {system 'rm $output_file2';}
  if (-f $output_file3) {system 'rm $output_file3';}
  symlink $INPUT0      ,  $input_file;
  symlink $OUTPUT_FILE0,  $output_file0;
  symlink $OUTPUT_FILE1,  $output_file1;
  symlink $OUTPUT_FILE2,  $output_file2;
  symlink $OUTPUT_FILE3,  $output_file3;
  symlink $output_file0,  $OUTPUT0;
  symlink $output_file1,  $OUTPUT1;
  symlink $output_file2,  $OUTPUT2;
  symlink $output_file3,  $OUTPUT3;
}
else {
}
if ($input_file) {
  my $cmd = "time root4star -b -q 'bfc.C(999,\"tfs\",\"" . $input_file . "\")\'";
  print $cmd,"\n";
  system 'echo $cmd';
  system 'echo "Finished job execution at" `date` " on " $HOSTNAME'; 
}
exit 0;
#______________________________________________________
sub convert($) {
  my $new_name = @_[0];
  $new_name =~ s'/home/starreco/reco'/disk00001/star'g;
  return $new_name;
}
#END
