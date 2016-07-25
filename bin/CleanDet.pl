#!/usr/bin/env perl
use Digest::MD5;
#use  Digest::Perl::MD5;
my @vers = qw(
  year2000
  year_2b
  year2001
  year2002
  year_2a
  year2003
  y2003x
  y2003a
  y2003b
  y2003c
  y2004
  y2004x
  y2004y
  y2004a
  y2004b
  y2004c
  y2004d
  y2005x
  y2005
  y2005b
  y2005c
  y2005d
  y2005e
  y2006
  y2007
  dev2005
  complete
  ist1
  upgr01
  upgr02
  upgr03
  upgr04
  upgr05
  upgr07
  upgr08
);
my @md5_modules = qw(Digest::MD5 MD5 Digest::Perl::MD5);
my $module;
print "________________________________________________________________________________\n";
for $module (@md5_modules) {
  eval "use $module";
  $_ = $module, last if ! $@;
}
$module = $_;
die "Cannot find any MD5 module from:  @md5_modules" if $@;

$md5 = new $module;
my $glb = "*.C";
my @Files = glob "$glb"; 
#print "Files: @Files\n";
my @tags = @ARGV;
if ($#tags < 0) {
  foreach my $file (@Files) {
    my ($tag,$vers,$ext) = split /\./, $file;
    print "$file => $tag $vers $ext\n";
    foreach my $t (@tags) {
      if ($t eq $tag) {goto NEXT;}
    }
    push @tags, $tag;
  NEXT:
  }
}
print "tags: @tags\n";

my $novers = $#vers + 1;
foreach my $tag (@tags) {
  for (my $i = 0; $i<$novers;$i++) {
    my $file = $tag . "." . $vers[$i] . ".C";
    print "$i $file\n";
    if (! -r $file) {next;} 
    open(FILE, "<$file") or die "Can't open $file\n";
    binmode(FILE);
    my $hex1 = Digest::MD5->new->addfile(*FILE)->hexdigest;
    print $hex1, "$i\t  $file\n";
    close(FILE);
    my $hex2 = 0;
    for (my $j = $i+1; $j<$novers; $j++) {
      my $file2 = $tag . "." . $vers[$j] . ".C";
      print "$j $file2\n";
      if (! -r $file2) {next;} 
      open(FILE, "<$file2") or die "Can't open $file2\n";
      binmode(FILE);
      $hex2 = Digest::MD5->new->addfile(*FILE)->hexdigest;
      print $hex2, "$j\t  $file2\n";
      close(FILE);
      if ($hex1 eq $hex2) {print "$file and $file2 are identical remove $file2\n"; unlink $file2;}
      else                {print "$file and $file2 are different\n"; $i = $j-1; goto ENDL;}
    }
  ENDL:
  }
}
