#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my $day = File::Basename::basename(File::Basename::dirname($pwd));
#my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2020/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
my $debug = 0;
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
my @Runs = (
'  0' => {trig=>'Cosmic',       field => 'UF',  first=> '20302016',      last => '20323008',     list => '',  beginTime => '20191029.200638'}, #        2019-10-29      20:06:38        2019-11-25      17:54:15
'  1' => {trig=>'Cosmic',       field => 'ZF',  first=> '20329025',      last => '20331016',     list => '',  beginTime => '20191125.175415'}, #        2019-11-25      17:54:15        2019-11-28      05:15:55
'  2' => {trig=>'Cosmic',       field => 'RF',  first=> '20332004',      last => '20336029',     list => '',  beginTime => '20191128.051555'}, #        2019-11-28      05:15:55        2019-12-02      15:43:23
'  3' => {trig=>'Cosmic',       field => 'ZF',  first=> '20336032',      last => '20339019',     list => '',  beginTime => '20191202.154323'}, #        2019-12-02      15:43:23        2019-12-05      16:02:46
'  4' => {trig=>'Cosmic',       field => 'RF',  first=> '20339022',      last => '20339041',     list => '',  beginTime => '20191205.160246'}, #        2019-12-05      16:02:46        2019-12-08      09:13:08
'  5' => {trig=>'11p5GeV',      field => 'RF',  first=> '20342002',      last => '20342006',     list => '',  beginTime => '20191208.091308'}, #        2019-12-08      09:13:08        2019-12-08      16:01:51
'  6' => {trig=>'Cosmic',       field => 'RF',  first=> '20342024',      last => '20343055',     list => '',  beginTime => '20191208.160151'}, #        2019-12-08      16:01:51        2019-12-10      05:09:42
'  7' => {trig=>'11p5GeV',      field => 'RF',  first=> '20344002',      last => '20345003',     list => '',  beginTime => '20191210.050942'}, #        2019-12-10      05:09:42        2019-12-11      06:42:48
'  8' => {trig=>'Cosmic',       field => 'RF',  first=> '20345009',      last => '20345009',     list => '',  beginTime => '20191211.064248'}, #        2019-12-11      06:42:48        2019-12-11      07:49:56
'  9' => {trig=>'11p5GeV',      field => 'RF',  first=> '20345011',      last => '20345035',     list => '',  beginTime => '20191211.074956'}, #        2019-12-11      07:49:56        2019-12-12      00:43:06
' 10' => {trig=>'Cosmic',       field => 'RF',  first=> '20345036',      last => '20345039',     list => '',  beginTime => '20191212.004306'}, #        2019-12-12      00:43:06        2019-12-12      02:46:11
' 11' => {trig=>'11p5GeV',      field => 'RF',  first=> '20345040',      last => '20346003',     list => '',  beginTime => '20191212.024611'}, #        2019-12-12      02:46:11        2019-12-12      09:59:27
' 12' => {trig=>'Cosmic',       field => 'RF',  first=> '20346010',      last => '20346015',     list => '',  beginTime => '20191212.095927'}, #        2019-12-12      09:59:27        2019-12-12      12:00:33
' 13' => {trig=>'11p5GeV',      field => 'RF',  first=> '20346016',      last => '20346018',     list => '',  beginTime => '20191212.120033'}, #        2019-12-12      12:00:33        2019-12-12      17:37:20
' 14' => {trig=>'Cosmic',       field => 'ZF',  first=> '20346025',      last => '20346034',     list => '',  beginTime => '20191212.173720'}, #        2019-12-12      17:37:20        2019-12-12      22:23:09
' 15' => {trig=>'11p5GeV',      field => 'RF',  first=> '20346051',      last => '20347037',     list => '',  beginTime => '20191212.222309'}, #        2019-12-12      22:23:09        2019-12-14      03:41:19
' 16' => {trig=>'Cosmic',       field => 'RF',  first=> '20347038',      last => '20347038',     list => '',  beginTime => '20191214.034119'}, #        2019-12-14      03:41:19        2019-12-14      04:11:44
' 17' => {trig=>'11p5GeV',      field => 'RF',  first=> '20347039',      last => '20347039',     list => '',  beginTime => '20191214.041144'}, #        2019-12-14      04:11:44        2019-12-14      05:29:05
' 18' => {trig=>'Cosmic',       field => 'RF',  first=> '20348001',      last => '20348001',     list => '',  beginTime => '20191214.052905'}, #        2019-12-14      05:29:05        2019-12-14      07:50:31
' 19' => {trig=>'11p5GeV',      field => 'RF',  first=> '20348010',      last => '20348024',     list => '',  beginTime => '20191214.075031'}, #        2019-12-14      07:50:31        2019-12-14      13:18:53
' 20' => {trig=>'Cosmic',       field => 'RF',  first=> '20348026',      last => '20348026',     list => '',  beginTime => '20191214.131853'}, #        2019-12-14      13:18:53        2019-12-14      13:30:55
' 21' => {trig=>'11p5GeV',      field => 'RF',  first=> '20348027',      last => '20349015',     list => '',  beginTime => '20191214.133055'}, #        2019-12-14      13:30:55        2019-12-15      15:56:21
);
#____________________________________________________________
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key (sort keys %$env ) {
    print "{ $key }\t=> {'$env->{$key}->{trig}', \tfield=>`$env->{$key}->{field}',\tfirst=>'$env->{$key}->{first}', \tlast=>'$env->{$key}->{last}', \tbeginTime=>'$env->{$key}->{beginTime}'\n";
  }
}
sub GoodRun($$) {
  my $env = shift;
  my $run = shift;
  print "GoodRun:: run = $run" if $debug;
  foreach my $key (sort keys %$env ) {
    if ($pwd !~ m/$env->{$key}->{trig}/) {next;}
    if ($pwd !~ m/$env->{$key}->{field}/) {next;}
    if ($run < $env->{$key}->{first}) {next;}
    if ($run > $env->{$key}->{last}) {next;}
    print " accepted\n" if ($debug);
    return $run;
  }
  print " rejected\n" if ($debug);
  return -1;
}
my $def = {@Runs};
PrintHash($def,"Runs") if ($debug);
my  @runs  = glob "/hlt/cephfs/daq/2019/???/* /hlt/cephfs/daq/2020/???/*";  print "runs = @runs\n" if ($debug);
foreach my $run (@runs) {
  my $r = File::Basename::basename($run);
  if (GoodRun($def,$r) < 0) {next;}
  foreach my $tag (qw(st_physics_20 hlt)) {
    my @files = glob $run . "/" . $tag . "*.daq"; print "files = @files\n" if ($debug);
    if ($#files < 0) {next;}
    #  print "files = @files\n";
    my $day = int ($r/1000 - 20000); #print "ru = $r => day = $day\n";
#    if ($day != / 107 and $day != 113 and $day != 169) {next;}
#    if ($day !~ m/158|160|179|180|181|182|183/) {next;}
    #  if ($r >= 20100000) {next;}
    my $NF = $#files;
    my $step = int $NF;
    print "run = $run. files = $NF, step = $step\n" if ($debug);
    if ($step < 1) {$step = 1;}
    print "run = $run. files = $NF, step = $step\n" if ($debug);
    for (my $i = 0; $i < $NF; $i = $i +  $step) {
      print "i = $i, step = $step \n" if ($debug);
      my $file = $files[$i];
      my $b = File::Basename::basename($file,".daq");
      #    print "$b\n" if ($debug);
      my $mufile = $b . ".MuDst.root";
      if (-r $mufile) {next;}
      my $blafile = $b . ".bla.root";
      if (-r $blafile) {next;}
      print "string:$file\n";
      $fNo++;
    }
  }
}
if (! $fNo) {die "Don't have input files\n";}

