#! /usr/bin/env perl
 use File::Basename;
 use Cwd;
# cd  ~/Pico/2014P16id; Prepare2014AuAu200.pl;
my $debug = 0;
sub rmkdir {
  (my $Dir = $_[0]) =~ s/\\/\//g;
  my @terms = split "/", $Dir;
  my $dir = "";
  foreach my $term (@terms) {
    if ($dir) {$dir .= "/";}
    $dir .= $term;
    if (!-d $dir && ! mkdir($dir,0755)) {die "$0: can't create directory File::Find::path ($!).\n";}
  }
}
my @List = glob "AuAu*2014.list";
foreach my $file (@List) {
  my $trig = File::Basename::basename($file,".list");
  print "file = $file => trig = $trig\n";
  open(In,"$file") or die "Can't open $file";
  my $line;
  while ($line = <In>) {
    print $line if ($debug);
    chop($line);
    my ($dir,$dum,$mudst) = split(":",$line);
    my $run = File::Basename::basename($dir);
    my $day = File::Basename::basename(File::Basename::dirname($dir));
    my $FileN = "";
    if ($dir =~ /^\/home\/starlib/) {$FileN = "root://xrdstar.rcf.bnl.gov:1095/";}
    $FileN .= $dir . "/" . $mudst;
    print "dir = |$dir|, mudst = |$mudst|, day = $day, run = $run => $FileN\n" if ($debug);

    my $DIR = "/gpfs02/eic/ayk/STAR/reco/Pico/2014/" . $trig . "/" . $day . "/" . $run;
    if (! -d $DIR) {`mkdir -p $DIR`;}
    my $L = $DIR . "/MuDst.list";  print "output $L\n" if ($debug);
    open (Out,">>$L") or die "Can't open $L";
    print "$FileN\n" if ($debug);
    print Out "$FileN\n";
    close (Out);
  }
  close(In); 
} 	
