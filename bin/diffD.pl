#!/usr/bin/env perl
if ($#ARGV <=0) {
  print "usage\n $0 Referece_directory list_of_file_to_be_compared in current directory which will be markerd as __NEW__\n";
  print "$0 \$STAR/StRoot/Sti `diff -q \$STAR/StRoot/Sti . | grep differ | awk '{print \$4}'`\n";
  exit 0;
}
my $RefDir = $ARGV[0]; print "RefDir = $RefDir\n";
for (my $i = 1; $i <= $#ARGV; $i++) {
  my $file = $ARGV[$i]; print "file $file\n";
  my $cmd = "diff -D__NEW__ " . $RefDir . "/" . $file . " " . $file . " > " . $file . ".NEW && mv $file $file.BAK && mv $file.NEW $file";
  print "$cmd\n";
  system($cmd);
}
