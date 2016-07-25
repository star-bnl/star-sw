#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $ev1 = 1;
my $ev2 = 500;
for (my $f = $ev1; $f <= $ev2; $f++) {
  my @fileList = glob "*GL_" . $f  . ".root";# print "fileList[$#fileList] = @fileList\n";
  if ($#fileList < 0) {
    #     print "string:$f:$ev1:$ev2\n";
    print "string:GL_$f.root:100\n";
  }
#  last;
}
