#! /usr/bin/env perl
my @list = qw(
  complete dev2005 ist1 pix1 upgr01 upgr02 upgr03 upgr04 upgr05 upgr06
  upgr07 upgr08 upgr09 upgr10 upgr11 upgr13 y2003a y2003b y2003c y2003x y2004a
  y2004b y2004 y2004c y2004d y2004x y2004y y2005b y2005 y2005c y2005d y2005e
  y2005f y2005x y2006a y2006b y2006 y2007 year2000 year2001 year2002 year2003 year_2a year_2b
);
foreach my $tag (@list) {
  my $file = "Geometry." . $tag . ".C";
  open(Out,">$file") or die "Can't create $file";
  print Out "#include \"CreateGeometry.h\"
TDataSet *CreateTable() {return CreateGeometry(\"$tag\");}
";
  close(Out);
}
