#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
use RunXXDefs;
my $pwd = cwd();
#my $day = File::Basename::basename(File::Basename::dirname($pwd));
#my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2020/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
my $debug = 1;
my $fNo = 0;
# foreach my $glob (@globs) {
#   my @files = glob $glob;
#   foreach my $file (@files) {# print "file = $file\n";
#     my $b = File::Basename::basename($file,".daq");
#     print "$b\n" if ($debug);
#     my $mufile = $b . ".MuDst.root";
#     if (-r $mufile) {next;}
#     print "string:$file\n";
#     $fNo++;
#   }
# }
#____________________________________________________________
#my $TableName = "TpcZCorrectionB";
my $TableName = "TpcLengthCorrectionMDF";
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  my $oldTrig = "";
  foreach my $key (sort keys %$env ) {
    if ($env->{$key}->{trig} !~ /GeV/) {next;}
    if ($env->{$key}->{trig} eq $oldTrig) {next;}
    $oldTrig = $env->{$key}->{trig};
#    print "{ $key }\t=> {'$env->{$key}->{trig}', \tfield=>`$env->{$key}->{field}',\tfirst=>'$env->{$key}->{first}', \tlast=>'$env->{$key}->{last}', \tbeginTime=>'$env->{$key}->{beginTime}'\n";
#    printf("%-20s %s\n",$env->{$key}->{trig},$env->{$key}->{beginTime});
    printf("ln -s TpcSecRowB.%s.root                     \tTpcSecRowB.%s.root\n",$env->{$key}->{trig},$env->{$key}->{beginTime});
#      my $fileN = $TableName . "." . $env->{$key}->{trig} . ".C";
#      if (-r $fileN) {
#        my $fileT = $TableName . "." .  $env->{$key}->{beginTime} . ".C";
#        if (-r $fileT) {
# 	 print "file $fileT has already existed\n";
#        } else {
# 	 my $cmd = "ls -s $fileN $fileT";
# 	 print "$cmd\n";
# 	 symlink $fileN, $fileT;
#        }
#      } else {
#        print "$fileN does not exist.\n";
#      }
  }
}
my $def = {@Runs};# print "Runs = @Runs\n";
PrintHash($def,"Runs") if ($debug);



