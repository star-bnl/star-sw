#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
my $dir = File::Basename::basename($DIR);
#if ($#ARGV < 0) {
#  print "Usage: $0 files='*.root' FilesPerJob='100' Out='hadd_files'\n";
#  exit 0;
#} 
my %ARG = (files => '*.root',
	   all => '1', 
           FilesPerJob => '40',
	   Out => 'hadd',
	   version => '.DEV2',
	   platform => '64b',
#	   platform => '32b',
#	   gcc => 'gcc451',
	   keep => 'yes',
	   prefix => '',
	   option => '',
	   debug  => '0'
	  );
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}

while (my ($key,$value) = each %ARG) {
  print  "$key=$value\n";
}
my @periods = ();
if ($ARG{all}) {
  @periods = ('All'   => {first => '0', second => '99999999', list => ''});
} else {
#   @periods = ('RFF'   => {first => '12148026', second => '12149054', list => ''},
# 	      'FF'    => {first => '12154050', second => '12159034', list => ''},
# 	      'ZeroF' => {first => '12166021', second => '12166045', list => ''}
# 	     );
# Run XIX
  @periods = (
        '0'  => {tag =>'tune_2019',                             first => '20049020',     second => '20055078',   list => ''}, #         2019-02-18      17:30:17        2019-02-24      22:11:32
        '1'  => {tag =>'production_19GeV_2019',                 first => '20056032',     second => '20056040',   list => ''}, #         2019-02-25      20:23:20        2019-02-25      23:38:39
        '2'  => {tag =>'tune_2019',                             first => '20057002',     second => '20057002',   list => ''}, #         2019-02-26      09:37:53        2019-02-26      09:37:53
        '3'  => {tag =>'production_19GeV_2019',                 first => '20057003',     second => '20067031',   list => ''}, #         2019-02-26      09:57:02        2019-03-08      14:27:12 
        '4'  => {tag =>'tune_7p7GeV_2019',       		first => '20067032',     second => '20067033',   list => ''}, #         2019-03-08      15:43:53        2019-03-08      16:03:05
        '5'  => {tag =>'production_19GeV_2019',  		first => '20067038',     second => '20070021',   list => ''}, #         2019-03-09      00:36:16        2019-03-11      13:14:20
        '6'  => {tag =>'tune_7p7GeV_2019',       		first => '20070022',     second => '20070035',   list => ''}, #         2019-03-11      18:41:51        2019-03-11      21:58:36
        '7'  => {tag =>'production_19GeV_2019',  		first => '20070036',     second => '20093036',   list => ''}, #         2019-03-12      01:00:56        2019-04-03      11:16:40
        '8'  => {tag =>'tune_14p5GeV_2019',      		first => '20093039',     second => '20094043',   list => ''}, #         2019-04-03      19:10:41        2019-04-04      08:06:35
        '9'  => {tag =>'tune_14p5GeV_loose',     		first => '20094044',     second => '20094047',   list => ''}, #         2019-04-04      08:32:08        2019-04-04      09:34:58
        '10' => {tag =>'production_14p5GeV_2019',               first => '20094048',     second => '20107026',   list => ''}, #         2019-04-04      09:44:05        2019-04-17      12:45:56
        '11' => {tag =>'production_7.3GeV_fixedTarget_2019',    first => '20107029',     second => '20107029',   list => ''}, #         2019-04-17      13:34:01        2019-04-17      13:34:01
        '12' => {tag =>'production_14p5GeV_2019',               first => '20107030',     second => '20113041',   list => ''}, #         2019-04-17      14:12:46        2019-04-23      14:26:57
        '13' => {tag =>'production_7.3GeV_fixedTarget_2019',    first => '20113042',     second => '20113048',   list => ''}, #         2019-04-23      15:54:18        2019-04-23      17:22:27
        '14' => {tag =>'production_14p5GeV_2019',               first => '20113051',     second => '20144012',   list => ''}, #         2019-04-23      18:11:58        2019-05-24      11:36:12
        '15' => {tag =>'tune_7p7GeV_2019',                      first => '20144020',     second => '20144032',   list => ''}, #         2019-05-24      20:00:29        2019-05-25      02:08:31
        '16' => {tag =>'production_14p5GeV_2019',               first => '20144038',     second => '20150009',   list => ''}, #         2019-05-25      03:53:12        2019-05-30      11:12:34
        '17' => {tag =>'tune_7p7GeV_2019',                      first => '20150019',     second => '20150023',   list => ''}, #         2019-05-30      16:55:40        2019-05-30      18:10:43
        '18' => {tag =>'production_14p5GeV_2019',               first => '20150029',     second => '20154013',   list => ''}, #         2019-05-31      03:45:00        2019-06-03      11:37:46
        '19' => {tag =>'tune_7p7GeV_2019',                      first => '20154018',     second => '20154046',   list => ''}, #         2019-06-03      15:49:29        2019-06-03      20:44:08
        '20' => {tag =>'production_7p7GeV_2019',                first => '20154047',     second => '20158026',   list => ''}, #         2019-06-03      22:36:55        2019-06-07      14:51:00
        '21' => {tag =>'production_3p85GeV_fixedTarget_2019',   first => '20158028',     second => '20158028',   list => ''}, #         2019-06-07      16:03:57        2019-06-07      16:03:57
        '22' => {tag =>'production_7p7GeV_2019',                first => '20158036',     second => '20158036',   list => ''}, #         2019-06-07      17:03:52        2019-06-07      17:03:52
        '23' => {tag =>'production_3p85GeV_fixedTarget_2019',   first => '20158040',     second => '20158042',   list => ''}, #         2019-06-07      17:41:23        2019-06-07      17:48:34
        '24' => {tag =>'production_7p7GeV_2019',                first => '20158047',     second => '20160022',   list => ''}, #         2019-06-07      20:14:27        2019-06-09      11:41:14
        '25' => {tag =>'production_3p85GeV_fixedTarget_2019',   first => '20160023',     second => '20160027',   list => ''}, #         2019-06-09      12:45:28        2019-06-09      13:44:07
        '26' => {tag =>'production_7p7GeV_2019',                first => '20160035',     second => '20169025',   list => ''}, #         2019-06-10      02:31:52        2019-06-18      12:30:19
        '27' => {tag =>'production_7.3GeV_fixedTarget_2019',    first => '20169028',     second => '20169055',   list => ''}, #         2019-06-18      14:36:58        2019-06-19      01:36:03
        '28' => {tag =>'production_7p7GeV_2019',                first => '20169058',     second => '20178014',   list => ''}, #         2019-06-19      02:51:26        2019-06-27      10:23:40
        '29' => {tag =>'tune_9p2GeV_2019',                      first => '20178059',     second => '20179014',   list => ''}, #         2019-06-27      22:57:55        2019-06-28      08:11:10
        '30' => {tag =>'production_9p2GeV_2019',                first => '20179015',     second => '20179028',   list => ''}, #         2019-06-28      08:19:31        2019-06-28      14:32:40
        '31' => {tag =>'production_4p59GeV_fixedTarget_2019',   first => '20179039',     second => '20183005',   list => ''}, #         2019-06-29      03:28:34        2019-07-02      05:39:51
        '32' => {tag =>'production_9p2GeV_2019',                first => '20183006',     second => '20183007',   list => ''}, #         2019-07-02      06:10:59        2019-07-02      06:17:04
        '33' => {tag =>'production_4p59GeV_fixedTarget_2019',   first => '20183008',     second => '20183025',   list => ''}, #         2019-07-02      06:22:10        2019-07-02      13:16:09
        '34' => {tag =>'production_9p2GeV_2019',                first => '20183026',     second => '20189017',   list => ''}, #         2019-07-02      14:41:51        2019-07-08      10:37:18
        '35' => {tag =>'tune_31GeV_fixedTarget_2019',           first => '20189030',     second => '20189032',   list => ''}, #         2019-07-09      02:51:17        2019-07-09      03:12:02
        '36' => {tag =>'production_31GeV_fixedTarget_2019',     first => '20189035',     second => '20189042',   list => ''}, #         2019-07-09      03:23:12        2019-07-09      03:55:52
        '37' => {tag =>'tune_31GeV_fixedTarget_2019',           first => '20190001',     second => '20190002',   list => ''}, #         2019-07-09      04:07:44        2019-07-09      04:09:28
        '38' => {tag =>'production_31GeV_fixedTarget_2019',     first => '20190006',     second => '20190024',   list => ''}, #         2019-07-09      04:16:30        2019-07-09      14:35:52
        '39' => {tag =>'production_AuAu200_2019',               first => '20190042',     second => '20190042',   list => ''}, #         2019-07-09      19:15:24        2019-07-09      19:15:24
        '40' => {tag =>'tune_AuAu200_2019',                     first => '20190044',     second => '20191003',   list => ''}, #         2019-07-10      00:24:24        2019-07-10      10:43:04
        '41' => {tag =>'production_AuAu200_2019',               first => '20191005',     second => '20193026',   list => ''}, #         2019-07-10      11:01:57        2019-07-12      19:56:37
        '42' => {tag =>'production_9p2GeV_2019',                first => '20196005',     second => '20196017',   list => ''}, #         2019-07-15      08:50:04        2019-07-15      11:43:38
	     );
}
my @tags = ();
my $def = {@periods};
foreach my $key (sort keys %$def) {
  my $found = 0;
  my $t =  $def->{$key}->{tag};
  print "key = $key => $t \n";
  foreach my $tag (@tags) {
#    print "\t    tag = $tag\n";
    if ($t eq $tag) {$found = 1; next;}
  }
  next if $found;
  push @tags, $t;
}
my $tags = join '|', @tags;
print "found tags = $#tags : tags   ==> $tags\n";
#if ($#tags < 0) {die "No tags found";}
my $glob = $ARG{files}; print "glob = $glob\n"; 
my $outn =$glob; $outn =~ s#\*##g; $outn =~ s#\.root##;
my $FilesPerJob = $ARG{FilesPerJob}; print "FilesPerJob = $FilesPerJob\n";
my $Out = $ARG{Out}; print "Out = $Out\n";
my @Files = glob "$glob"; 
print "no of files : $#Files\n"; 
if ($#Files < 0) {die "No files fond";}
my $fno = 0;
foreach my $file (@Files) { 
  if ($file eq '') {next;}
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, 
      $mtim, $ctime, $blksize,$blocks) = stat($file );
#  next if $size < 500000; # 0.5 MB limit
  my $f = File::Basename::basename($file);# print "$file";
  my $dir = File::Basename::dirname($file);# print "$file";
  next if ($f =~ /^($tags)/);
  $f =~ s/\.root//g;
  $f =~ s/adc_//g;
  $f =~ s/st_//;
  $f =~ s/W_//;
  $f =~ s/tofcosmic_//;
  $f =~ s/hlt_//;
  $f =~ s/physics_//;
  my @ss = split /_/, $f; 
  $f = $ss[0]; print " $file ==> $f\n" if ($ARG{debug});
  if (!$f) {next;}
#   foreach my $r (@runXIbadList) {
# #    if ($f =~/12112053/) {print "match $f with $r\n";}
#     if ($f =~ /$r/) {
#       print "Found bad run $r matched with $f in $file, skip it \n";
#       goto ENDL;
#     }
#   }
#  foreach my $key (sort keys %$def) {
  my $found = 0;
  foreach my $key (keys %$def) {
    if ($f >= $def->{$key}->{first} and $f <= $def->{$key}->{second}) {
      if (! $def->{$key}->{list}) { $def->{$key}->{list} =                              $file; }
      else                        { $def->{$key}->{list} = $def->{$key}->{list} . ' ' . $file; }
      print "$f : $key => ( $def->{$key}->{tag}, $def->{$key}->{first} - $def->{$key}->{second})  => $def->{$key}->{list}\n" if ($ARG{debug});
      $found++;
      last;
     } else {
       print "$f $def->{$key}->{tag} : $key  is not in  ( $def->{$key}->{first} - $def->{$key}->{second})\n" if ($ARG{debug});
    }
  }
  die "|$file| run = |$f| has not been found" if (! $found);
 ENDL:
}
my %Taglist = ();

foreach my $tag (@tags) {
#  print "tag = $tag\n";
  foreach my $key (sort keys %$def) {
    my $t = $def->{$key}->{tag};
#    print "key $key => $t  / $r \n";
    if ($t eq $tag){
      $TagList{$tag} .= " " . $def->{$key}->{list};
#      print "$tag => $TagList{$tag}\n";
    }
  }
}
my $XML = "hadd" . $outn . ".xml";
open (XML,">$XML") or die "Can't open $XML";
#
print XML '<?xml version="1.0" encoding="utf-8" ?> 
<job name="Hadd" maxFilesPerProcess="1" filesPerHour="10" simulateSubmission="false" fileListSyntax="paths" copyInputLocally="false" >
     <command>csh -x $INPUTFILE0 </command>
     <stdout URL="file:' . $DIR . '/sched$JOBID.log" />
';
foreach my $key (sort keys %TagList) {
  #  print "\n\n";
#  print "$key => $TagList{$key}\n";
  my @ListAll = split ' ', $TagList{$key};
  my $NJB = ($#ListAll+1)/$FilesPerJob+1;
  my $j = 0;
  for (my $jb = 1; $jb <= $NJB; $jb++) {
    my $i = 0;
    my @List = ();
    for (; $i< $FilesPerJob && $j <= $#ListAll; $i++, $j++) {
      push @List, $ListAll[$j];
    }
    next if  $#List == -1;
    my $list = join ' ', @List;
#    print "list => $list\n";
#    print "list => @List\n";
#    print "======================> $List[0] - $List[$#List]\n";
#    my @be = (File::Basename::basename($List[0]), File::Basename::basename($List[$#List]));
#    for (my $i = 0; $i < 2; $i++) {
#      my $f = $be[$i];
#      $f =~ s/\.root//g;
#      $f =~ s/adc_//g;
#      #      my @ss = split /_/, $f;
#      #      $f = $ss[0];
#      $be[$i] = $f; 
#    }
#    print "b/e => $be[0] - $be[1]";
#    my $job = $be[0] . "_" . $be[1];
    my $job = $jb;
    my $name     = $ARG{prefix} . $key . "_". $dir . $outn . "_" . $job;
    my $rootfile = $name. ".root";
    my $log      = $name. ".log";
    if ( -r $rootfile ) {print  "\tDone\n"; next;}
    else {print "\n";}
#    $cmd  = " test -r $rootfile ||  root.exe -q -b " . $list;
#    $cmd .= " Hadd.C\\\\(\\\\\"" . $rootfile . "\\\\\"\\\\)";
#    $cmd .= " 'Hadd.C(\"" . $rootfile . "\")'";
#    my $cmd = "test -r $rootfile || hadd -T -f $rootfile $list";
    my $cmd = "test -r $rootfile || hadd -k $ARG{option} -f $rootfile $list";
#    my $cmd = "test -r $rootfile || root.exe -q -b 'Hadd.C+(\"" . $rootfile . "\",\"" . $list . "\")'";
    $cmd .= ">&  $log";
    print "job:$jb files: $i => $cmd \n";
    my $SCRIPT = $name . ".csh";
#    next if -r $SCRIPT;
    open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
    print "Create $SCRIPT\n";
    print OUT "#!/bin/tcsh -v\n";
    print OUT "cd $DIR\n";
    print OUT "setenv NODEBUG yes\n";
    print OUT "setup " . $ARG{platform} . "\n";
#    print OUT "setup " . $ARG{gcc} . "\n";
    print OUT "starver " . $ARG{version} . "\n";
    print OUT "$cmd\n";
    if ($ARG{keep} eq 'no') {
      print OUT "if (\$? == 0) rm $list;\n";
    }
    close (OOUT);
    print XML "<input URL=\"file:" . $DIR . "/" .  $SCRIPT ."\" />\n";
  }
}
print XML '
</job>
';
close (XML);

