#!/opt/star/bin/perl
#
# Script to preprocess StCollection macros for rootcint
#
#
my $file = @ARGV[0];
my $line;
my $macro = `echo \$STAR/StRoot/St_base/StArray.h`;
open (INPUT, $file);
while ($line = <INPUT>) {
  if ($line =~ /StCollectionDef/) {
    my @Class = split /([\(\)])/, $line;
    my $class = $Class[2];
    open(DEF, $macro) || die "Can't open Macros $macro \n";
    my $def = 0;
    while ($line = <DEF>) {
      if ($line =~ /\#define/ && $line =~ /StCollectionDef/) {
        $def = 1; next;
      }
      if ($def && $line =~ /\#define/) {last;}
      if (! $def) { next; }
      $line =~ s/\\//g;
      $line =~ s/QWERTY/$class/g;
      $line =~ s/ \#\# //g;
      $line =~ s/\#\# //g;
      $line =~ s/ \#\#//g;
      print $line;
    }
  }
  else {print $line;}
}

exit(0);
# last line
