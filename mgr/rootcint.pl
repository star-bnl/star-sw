#!/opt/star/bin/perl
#use File::Basename;
#print "rootcint @ARGV\n";
my $DirName = shift;
(my $dir = $DirName) =~ s/\S*\//\.\.\//g;# print "DirName = $DirName dir = $dir\n";
$dir =~ s/\/\S*/\/\.\./g;# print "dir = $dir\n";
my @argv = ();
foreach my $a (@ARGV) {
  foreach my $c (split / /, $a) {
    if ($c  !~ /^-I/ || $c =~ /^-I\//) {push @argv, $c;}
    else {(my $cc = $c) =~ s/-I//; $cc = "-I" . $dir . "/" . $cc; push @argv, $cc;}
  }
}
my $com = "cd $DirName && rootcint @argv"; print "$com\n";
exit `$com`;
