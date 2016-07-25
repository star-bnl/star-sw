#! /usr/bin/env perl
use strict;
use File::Basename;
if ($#ARGV < 0) {
  print "Usage: $0 depends_file\n";
  exit 0;
}
my $DepsFile = $ARGV[0];
my $DepFlag = "";
if ($#ARGV > 0) {$DepFlag = $ARGV[1];}
open (IN,"$DepsFile") or die "Can't open $DepsFile";
my $html = "/afs/rhic/star/users/fisyak/WWW/GraphLayout/star_deps" . $DepFlag .".html";
open (OUT,">$html") or die "Can't open $html";
print OUT '
<html>
  <head>
      <title>STAR offline library dependencies layout </title>
  </head>
  <body>
    <h1>STAR offline library dependencies layout</h1>
      <hr>
      <applet code="Graph.class" width=800 height=800>
	<param name=edges value="
';
my $line;
my $i = 0;
my @skip_libs = qw(
		   ASImageGui  Ged          GX11TTF      Netx        QtGui    Table       XrdPosix
		   ASImage     GeomPainter  Hbook        New         QtImage  Thread      XrdRootd
		   Cint        Geom         HistPainter  PgSQL       QtRoot   TreePlayer  XrdSeckrb4
		   Core        Gpad         Hist         Physics     QtX3d    Tree        XrdSeckrb5
		   EGPythia6   GQt          Html         Postscript  Quadp    TreeViewer  XrdSec
		   EGPythia    Graf3d       Krb5Auth     ProofGui    RGL      VMC
		   EG          Graf         Matrix       Proof       Rint     X3d
		   Foam        GuiBld       Minuit       PyROOT      RLDAP    XMLIO
		   Fumili      Gui          MLP          QGLViewer   RQTGL    XMLParser
		   GedOld      GX11         MySQL        QtGed       SrvAuth  XrdOfs
		   geant3 minicern geometryNoField);

my $skip_libs = join '|', @skip_libs;
while ($line = <IN>) {
  next if $line !~ /\:/;
  my ($lib,$deps) = split ':', $line;
  my $Lib = File::Basename::basename($lib);
  $Lib =~ s/^lib//;
  $Lib =~ s/\.so//;
  my @dlibs = split ' ',$deps;
  next if $Lib !~ /^St/;
  next if $#dlibs < 0;
  foreach my $d (@dlibs) {
    my $dep = File::Basename::basename($d);
    $dep =~ s/^lib//;
    $dep =~ s/\.so//;
    if ($Lib =~ /$skip_libs/ or $dep =~ /$skip_libs/) {next;}
    next if $dep !~ /^St/;
    if ($DepFlag and ($Lib !~ /^$DepFlag/ or $dep !~ /^$DepFlag/)) {next;}
#    next if $dep =~ /geant3/ or $dep =~ /minicern/;
#    next if $dep =~ /Xrd/ or $dep =~ /X11/ or  $dep =~ /Auth/ or $dep = 
    if ($i > 0) {print OUT ",";}
    $i++;
    print OUT $Lib, "-", $dep;
  }
}
print OUT "\">\n";
close (IN);
print OUT '
	<param name=center value="StEvent">
        alt="Your browser understands the &lt;APPLET&gt; tag but isn\'t running the applet, for some reason."
	Your browser is completely ignoring the &lt;APPLET&gt; tag!
      </applet>
      <hr>
      <a href="Graph.java">The source</a>.
  </body>
</html>
';
close (OUT);
