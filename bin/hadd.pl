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
my @periods = ();
my $all = 1;
if ($all) {
  @periods = ('All'   => {first => '0', second => '99999999', list => ''});
} else {
  @periods = ('RFF'   => {first => '12148026', second => '12149054', list => ''},
	      'FF'    => {first => '12154050', second => '12159034', list => ''},
	      'ZeroF' => {first => '12166021', second => '12166045', list => ''}
	     );
}
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}

while (my ($key,$value) = each %ARG) {
  print  "$key=$value\n";
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
print "found tags = $#tags : @tags   ==> $tags\n";
#if ($#tags < 0) {die "No tags found";}
my $glob = $ARG{files}; print "glob = $glob\n"; 
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
my $XML = "hadd.xml";
open (XML,">$XML") or die "Can't open $XML";
#
print XML '<?xml version="1.0" encoding="utf-8" ?> 
<job name="Hadd" maxFilesPerProcess="1" filesPerHour="10" simulateSubmission="false" fileListSyntax="paths">
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
    my $name     = $ARG{prefix} . $key . "_". $dir . "_" . $job;
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

