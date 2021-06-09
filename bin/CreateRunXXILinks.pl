#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
use RunXXIDefs;
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
    printf("ln -sf tpcT0BX.%s.C                     \ttpcT0BX.%s.C\n",$env->{$key}->{trig},$env->{$key}->{beginTime});
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
#
# ln -sf tpcT0BX.7p7GeV_2021.C                            tpcT0BX.20210131.193346.C # 20210129.000000 2021-05-01 12:58:29 GMT
# ln -sf tpcT0BX.3p85GeV_fixedTarget_2021.C               tpcT0BX.20210501.165437.C # 20210501.160000 2021-05-05 11:30:40 GMT
# ln -sf tpcT0BX.44p5GeV_fixedTarget_2021.C               tpcT0BX.20210506.070146.C # 20210506.000000 2021-05-06 18:27:53 GMT
# ln -sf tpcT0BX.70GeV_fixedTarget_2021.C                 tpcT0BX.20210507.031639.C # 20210507.000000 2021-05-07 16:04:17 GMT 
# ln -sf tpcT0BX.100GeV_fixedTarget_2021.C                tpcT0BX.20210508.041526.C # 20210508.000000 2021-05-08 14:23:30 GMT
# ln -sf tpcT0BX.OO_200GeV_2021.C                         tpcT0BX.20210510.134726.C # 20210510.000000 2021-05-16 11:19:05 GMT < 
# ln -sf tpcT0BX.ps_OO_200GeV_2021.C                      tpcT0BX.20210513.083120.C # 20210513.000000 2021-05-21 11:20:31 GMT <
# ln -sf tpcT0BX.OO_200GeV_2021.C                         tpcT0BX.20210513.084422.C # 20210513.
# ln -sf tpcT0BX.ps_OO_200GeV_2021.C                      tpcT0BX.20210513.175228.C # 20210513.
# ln -sf tpcT0BX.OO_200GeV_2021.C                         tpcT0BX.20210513.182205.C # 20210513.
# ln -sf tpcT0BX.ps_OO_200GeV_2021.C                      tpcT0BX.20210516.112015.C # 20210516.
# ln -sf tpcT0BX.FF_OO_200GeV_2021.C                      tpcT0BX.20210522.023642.C # 20210522.000000 
# ln -sf tpcT0BX.17p3GeV_2021.C                           tpcT0BX.20210525.113236.C # 20210525.000000 2021-06-07 20:05:14 GMT
# ln -sf tpcT0BX.26p5GeV_fixedTarget_2021.C               tpcT0BX.20210604.023045.C # 20210604.000000 2021-06-04 04:10:11 GMT 
# ln -sf tpcT0BX.17p3GeV_2021.C                           tpcT0BX.20210604.081455.C # 20210604.000000 2021-06-07 20:05:14 GMT
# ln -sf tpcT0BX.3p85GeV_fixedTarget_2021.C               tpcT0BX.20210608.004951.C # 20210608.000000 
