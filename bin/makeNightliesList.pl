#! /usr/bin/env perl
use File::Basename;
use File::Find;
use Cwd;
my @use = qw(
	      grep 'Processing bfc.C' /star/rcf/test/dev/*/*/*/*/*.log | \
	      sed -e 's/daq_sl302//' -e 's/simu/MC/' -e 's/trs_sl302//' \
	      -e 's/_opt//' -e 's/ittf//' -e 's/stica//' -e 's/stihr//' \
	      -e 's/bfc\.C(//' -e 's/","/:/' -e 's/")\.\.\.//' | tee bfc1.log | \
	      awk -F\/ '{print $8"/"$6"/"$9" "$10}' | awk '{print $1":"$3}' | sort -u | tee Nightlies.listRaf);
my $today = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];
my $glob = "/star/rcf/test/dev/*.ittf/" . $today . "/*/*/*.log";
my @Files = glob $glob;# print "Files = @Files\n";
my %Hash = ();
my $FullPath = "";
my $Input = "";
sub input_file {
#  print "input_file: $file => $File::Find::name $_ \n";
  return unless -r $_ && $File::Find::name =~ $Input;
  return if $_ =~ /\.root/;
  $FullPath = $File::Find::name;
#  print "$_ = > $FullPath\n";
}
my $no = 0;
foreach my $file (@Files) {
#  print "$file\n";
  open(In,$file) or die "Can't open $file";
  while ( my $it = <In>) {
    if ($it !~ /Processing bfc/) {next;}
    my ($NoEvents,$Chain,$dum,$input) = split('"', $it);
    $Input = $input;
    $NoEvents =~ s/Processing bfc\.C\(//; $NoEvents =~ s/,//;
#    print "$file => NoEvents = $NoEvents; Chain = $Chain; input = $Input\n";
    $Hash{$file}->{file} = $file;
    $Hash{$file}->{NoEvents} = $NoEvents;
    $Hash{$file}->{Chain} = $Chain;
    $Hash{$file}->{input} = $Input;
    $FullPath = "";
    File::Find::find(\&input_file, qw(/star/rcf/test/daq /star/rcf/test/gstardata /star/rcf/simu));
    $Hash{$file}->{FullPath} = $FullPath;
#    print "$Hash{$file}->{file} => NoEvents = $Hash{$file}->{NoEvents}; Chain = $Hash{$file}->{Chain}; input = $Hash{$file}->{input}; Full Path = $Hash{$file}->{FullPath}\n";
    if (! $FullPath) {
      die;
    }
    last;
  } 
  $no++;
#  if ($no > 5) {
#    last;
#  }
} 
#die;
#my $File = "Nightlies.listRaf";
#open(In, $File) or die "Can't open $File";
#while ( my $it = <In>) {
foreach my $ref ( sort keys %Hash ) {
  my $Dir1 = File::Basename::dirname($ref);  my $dir1 = File::Basename::basename($Dir1);
  my $Dir2 = File::Basename::dirname($Dir1); my $dir2 = File::Basename::basename($Dir2);
  my $Dir  = $dir2 . "/" .$dir1;
#  print "$ref => $dir2 $dir1 => $Dir\n";
  if ($ref =~ /daq/) {
    $Dir =~ s/\//\/RC\./; 
  } else {
    $Dir =~ s/\//\/MC\./;
  }
  my $string = "string:" .$Hash{$ref}->{Chain} . ":" . $Dir . ":" . $Hash{$ref}->{FullPath} . ":" . $Hash{$ref}->{NoEvents} . ":" .$ref;
  print "$string\n";   
}

