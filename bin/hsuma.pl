#! /usr/bin/env perl
use Cwd;
use lib "/afs/rhic/star/users/fisyak/public/.dev";
use JobSubmit;
my $dir =  File::Basename::basename(Cwd::cwd());
print "dir = $dir\n";
my $jobs = 0;
my $glb = "";
if ($#ARGV >=0 ) {
  $glb = $ARGV[0];
} else {
  $glb = "[0-9]*.root adc_[0-9]*.root";
}
my @Files = glob $glb;
print "no of files $glb: $#Files\n";
my @periods = ();
my $all = 1;
if ($all) {
 @periods = ('All'   => {first => '0', second => '9999999', list => ''});
}
# else {
#  @periods = ('AuAu_1'   => {first => '5004009', second => '5034402', list => ''}, #20040104.040000#  01/04/04  4:05 -
#	      'AuAu_2'   => {first => '5036041', second => '5047064', list => ''}, #20040205.090000# 02/05/04  9:42 - 02/16/04 18:49
#	      'AuAu_3'   => {first => '5048018', second => '5084044', list => ''}, #20040217.160000# 5131020), # 02/17/04 16:27 - 05/10/04 21:37
#	      'AuAu_62'  => {first => '5084045', second => '5093007', list => ''}, #20040324.210000# 03/24/04 21:32 - 04/02/04 01:42
#	      'pp'       => {first => '5095001', second => '5135070', list => ''});#20040404.200000# 04/04/04 20:25 - 95/14/20:03
#  @periods = ('pp_1'   => {first => '7041020', second => '7104016', list => ''}, # 20060308.115800 tpcGain 20060308.115800 - before mag. shutdown
#	      'pp_2'   => {first => '7114063', second => '7131000', list => ''}, # 20060424.210000 after  -"-           20060424.214105 
#              'pp_3'   => {first => '7131000', second => '7139017', list => ''}, # 20060510.150600 tpcGain table change 20060510.150600
#              'pp_4'   => {first => '7139017', second => '7171011', list => ''});# 20060519.160000 after gas refill      
#  @periods = ('pp_A'   => {first => '7041020', second => '7129000', list => ''}, # tpcGain change on 2006-05-10 15:06:00 
#              'pp_B'   => {first => '7131000', second => '7171011', list => ''}); 
#}
my @Run   = sort keys %runs;
my $FilesPerJob = 40;
my $def = {@periods};
my $fno = 0;
foreach my $file (@Files) {
  my $f = File::Basename::basename($file);# print "$file";
  $f =~ s/\.root//g;
  $f =~ s/adc_//g;
  my @ss = split /_/, $f; 
  $f = $ss[0];# print " ==> $f\n";
  
  foreach my $key (sort keys %$def) {
#    print "$f : $key => ( $def->{$key}->{first} - $def->{$key}->{second}) \n";
    if ($f >= $def->{$key}->{first} and $f <= $def->{$key}->{second}) {
      if (! $def->{$key}->{list}) { $def->{$key}->{list} =                              $file; }
      else                        { $def->{$key}->{list} = $def->{$key}->{list} . ' ' . $file; }
    }
  }
}
foreach my $key (sort keys %$def) {
#  print "\n\n";
#  print "$key => ( $def->{$key}->{first} - $def->{$key}->{second}) = |$def->{$key}->{list}|\n";
  my @ListAll = split ' ', $def->{$key}->{list};
  my $NJB = ($#ListAll+1)/$FilesPerJob+1;
  my $j = 0;
  for (my $jb = 1; $jb <= $NJB; $jb++) {
    my $i = 0;
    my @List = ();
    for (; $i< $FilesPerJob && $j <= $#ListAll; $i++, $j++) {
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, 
          $mtim, $ctime, $blksize,$blocks) = stat($ListAll[$j] );
      next if $size < 500000; # 0.5 MB limit
      push @List, $ListAll[$j];
    }
    next if  $#List == -1;
    my $list = join ' ', @List;
 #   print "list = $list\n";
 #   print "List = @List\n";
 #   print "======================> $List[0] - $List[$#List]\n";
    
    my @be = (File::Basename::basename($List[0]), File::Basename::basename($List[$#List]));
    for (my $i = 0; $i < 2; $i++) {
      my $f = $be[$i];
      $f =~ s/\.root//g;
      $f =~ s/adc_//g;
#      my @ss = split /_/, $f;
#      $f = $ss[0];
      $be[$i] = $f; 
    }
    print "b/e => $be[0] - $be[1]";
    my $job = $be[0] . "_" . $be[1];
    my $rootfile = $dir ."_" . $key . "_" . $job . ".root";
    my $log  = $job . ".log";
    if ( -r $rootfile ) {print  "\tDone\n"; next;}
    else {print "\n";}
#    my $cmd = " bsub -o $log -N -q star_cas_big";
    my $cmd = " bsub -o $log -N -q star_cas_prod";
    $cmd .= " root.exe -q -b " . $list;
    #  $cmd .= " '/afs/rhic/star/users/fisyak/.dev/Hadd.C(\"" . $rootfile . "\")'";
    $cmd .= " 'Hadd.C(\"" . $rootfile . "\")'";
    #  $cmd .= ">& " . $dir ."_" . $jb . ".log";
    print "job:$jb files: $i => $cmd \n";
    my $flag = system($cmd);
  }
}
