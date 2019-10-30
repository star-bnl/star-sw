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
	   option => ''
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
	      'production_19GeV_2019'                            => {first => '20056032', second => '20093036', list => ''}, #        2019-02-25      20:23:20        2019-04-03      11:16:40
	      'production_14p5GeV_2019'                          => {first => '20094048', second => '20107026', list => ''}, #         2019-04-04      09:44:05        2019-04-17      12:45:56
	      'production_7.3GeV_fixedTarget_2019'               => {first => '20107029', second => '20107029', list => ''}, #         2019-04-17      13:34:01        2019-04-17      13:34:01
	      'production_14p5GeV_2019'                          => {first => '20107030', second => '20113041', list => ''}, #        2019-04-17      14:12:46        2019-04-23      14:26:57
	      'production_7.3GeV_fixedTarget_2019'               => {first => '20113042', second => '20113048', list => ''}, #        2019-04-23      15:54:18        2019-04-23      17:22:27
	      'production_14p5GeV_2019'                          => {first => '20113051', second => '20154013', list => ''}, #        2019-04-23      18:11:58        2019-06-03      11:37:46
	      'production_7p7GeV_2019'                           => {first => '20154047', second => '20158026', list => ''}, #        2019-06-03      22:36:55        2019-06-07      14:51:00
	      'production_3p85GeV_fixedTarget_2019'              => {first => '20158028', second => '20158028', list => ''}, #        2019-06-07      16:03:57        2019-06-07      16:03:57
	      'production_7p7GeV_2019'                           => {first => '20158036', second => '20158036', list => ''}, #        2019-06-07      17:03:52        2019-06-07      17:03:52
	      'production_3p85GeV_fixedTarget_2019'              => {first => '20158040', second => '20158042', list => ''}, #        2019-06-07      17:41:23        2019-06-07      17:48:34
	      'production_7p7GeV_2019'                           => {first => '20158047', second => '20160022', list => ''}, #        2019-06-07      20:14:27        2019-06-09      11:41:14
	      'production_3p85GeV_fixedTarget_2019'              => {first => '20160023', second => '20160027', list => ''}, #        2019-06-09      12:45:28        2019-06-09      13:44:07
	      'production_7p7GeV_2019'                           => {first => '20160035', second => '20169025', list => ''}, #        2019-06-10      02:31:52        2019-06-18      12:30:19
	      'production_7.3GeV_fixedTarget_2019'               => {first => '20169028', second => '20169055', list => ''}, #        2019-06-18      14:36:58        2019-06-19      01:36:03
	      'production_7p7GeV_2019'                           => {first => '20169058', second => '20178014', list => ''}, #        2019-06-19      02:51:26        2019-06-27      10:23:40
	      'production_9p2GeV_2019'                           => {first => '20179015', second => '20179028', list => ''}, #        2019-06-28      08:19:31        2019-06-28      14:32:40
	      'production_4p59GeV_fixedTarget_2019'              => {first => '20179039', second => '20183005', list => ''}, #        2019-06-29      03:28:34        2019-07-02      05:39:51
	      'production_9p2GeV_2019'                           => {first => '20183006', second => '20183007', list => ''}, #        2019-07-02      06:10:59        2019-07-02      06:17:04
	      'production_4p59GeV_fixedTarget_2019'              => {first => '20183008', second => '20183025', list => ''}, #        2019-07-02      06:22:10        2019-07-02      13:16:09
	      'production_9p2GeV_2019'                           => {first => '20183026', second => '20189017', list => ''}, #        2019-07-02      14:41:51        2019-07-08      10:37:18
	      'production_31GeV_fixedTarget_2019'                => {first => '20189035', second => '20190024', list => ''}, #         2019-07-09      03:23:12        2019-07-09      14:35:52
	      'production_AuAu200_2019'                          => {first => '20190042', second => '20193026', list => ''}  #         2019-07-09      19:15:24        2019-07-12      19:56:37
	     );
}
my @tags = ();
my $def = {@periods};
foreach my $key (sort keys %$def) {
  my $found = 0;
  print "key = $key\n";
  my ($t,$r) = split ':', $key;
  foreach my $tag (@tags) {
    print "\t    tag = $tag\n";
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

#my @runXIbadList  = qw (
#12111020 12111021 12111023 
#);
foreach my $file (@Files) { 
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
  $f = $ss[0]; # print " $file ==> $f\n";
  foreach my $r (@runXIbadList) {
#    if ($f =~/12112053/) {print "match $f with $r\n";}
    if ($f =~ /$r/) {
      print "Found bad run $r matched with $f in $file, skip it \n";
      goto ENDL;
    }
  }
#  foreach my $key (sort keys %$def) {
  foreach my $key (keys %$def) {
    if ($f >= $def->{$key}->{first} and $f <= $def->{$key}->{second}) {
      if (! $def->{$key}->{list}) { $def->{$key}->{list} =                              $file; }
      else                        { $def->{$key}->{list} = $def->{$key}->{list} . ' ' . $file; }
#      print "$f : $key => ( $def->{$key}->{first} - $def->{$key}->{second})  => $def->{$key}->{list}\n";
      last;
    } else {
#      print "$f : $key is not in  ( $def->{$key}->{first} - $def->{$key}->{second})\n";
    }
  }
 ENDL:
}
#die;
my %Taglist = ();
foreach my $tag (@tags) {
#  print "tag = $tag\n";
  foreach my $key (sort keys %$def) {
    my ($t,$r) = split ':', $key;
#    print "key $key => $t  / $r \n";
    if ($t eq $tag){
      $TagList{$tag} .= " " . $def->{$key}->{list};
#      print "$tag => $TagList{$tag}\n";
    }
  }
}
#die;
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

