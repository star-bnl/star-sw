#!/usr/local/bin/perl

# Originally written by David Anderson (dva@ee.gatech.edu) Oct 7, 1997

# This program is called with the base filename of the .ps file as the
# first argument and the title of the html document as the second
# argument.  For example, to convert resume.ps I would type:
#  ps2www resume "Resume of David Anderson"

# $Log: ps2www.pl,v $
# Revision 1.1  2000/01/26 16:04:02  fisyak
# Add ps2www.pl
#
# Revision 1.1  1998/05/06 16:00:46  dva
# Initial revision
#

require 'getopts.pl';

$USAGE = "Usage:  ps2www [-l -s -d directory -t directory] filename title\n" . 
" Where filename is the name of the .ps file *without* the .ps extension\n" .
" and title is the title of the HTML document to be produced.\n" .
"      -l              = landscape mode\n" .
"      -s              = seascape mode\n" .
"      -c              = crop white space\n" .
"      -p              = force creation of PDF file\n" .
"      -d directory    = destination directory\n" .
"      -t directory    = temporary directory\n";

$tmpdir = "tmp_";
$destdir= "./";

if ($#ARGV < 1) {
    print STDERR $USAGE;
    exit 1;
}

do Getopts('lscpt:d:');  # -o, -D & -I take arg.  Sets opt_* as a side effect.

$base = $ARGV[0];
$title = $ARGV[1];

if (! -e "$base.ps") {
    print STDERR "file not found: $base.ps\n";
    print STDERR $USAGE;
    exit 1;
}

if ($opt_t) {
    $tmpdir=$opt_t;
}
if ($opt_d) {
    $destdir="$opt_d/";
    if (! -e "$destdir") {
	system "mkdir $destdir";
    }
    system "cp $base.ps $destdir$base.ps";
}
$xtra_cmds = "";

if ($opt_c) {
    $xtra_cmds = "pnmcrop -white | pnmpad -white -l5 -r5 -t5 -b5 |";
}
if ($opt_l) {
    $xtra_cmds = "$xtra_cmds pnmrotate 90 | ";
}
if ($opt_s) {
    $xtra_cmds = "$xtra_cmds pnmrotate -90 | ";
}

$commandstring="gs -dNOPAUSE -sDEVICE=ppmraw -sOutputFile=$tmpdir$base.%d.ppm -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -r100 $base.ps -c quit";

open(GS,"|$commandstring");
close(GS);

if (($opt_l || $opt_s) && ! -e "$destdir$base.pdf") {
    print "Warning, this script can only create PDF files\n" .
	" from portrait oriented originals.\n";
} elsif ((! -e "$destdir$base.pdf") || $opt_p) {
    if (! $opt_p) {
        print "No PDF file exists, creating PDF file\n";
    } else {
        print "Creating PDF file\n";
    }
    system "gs -q -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=$destdir$base.pdf $base.ps -c quit ";
}

open(INDEX,"| cat > $destdir$base.html");
print INDEX "<HTML>\n<HEAD>\n  <TITLE>$title</TITLE>\n</HEAD>\n";
print INDEX "<BODY BGCOLOR=\"#FFFFFF\">\n";
print INDEX "<CENTER><H1>$title</H1></CENTER>\n<HR>\n";
print INDEX "<p>Here is a <A HREF=\"$base.ps\">Postscript version</A>";
if (-e "$destdir$base.pdf") {
    print INDEX " and here is a <A HREF=\"$base.pdf\">PDF version</A>.\n";
    print INDEX "<p>You may obtain <A HREF=\"http://www.cs.wisc.edu/";
    print INDEX "~ghost/\">Ghostscript and Ghostview</A> for viewing and \n";
    print INDEX "printing the Postscript version.  The PDF version may be \n";
    print INDEX "viewed and printed using <A HREF=\"http://www.adobe.com/\">";
    print INDEX "Adobe Acrobat Reader</A>.\n";
} else {
    print INDEX ".\n";
    print INDEX "<p>You may obtain <A HREF=\"http://www.cs.wisc.edu/";
    print INDEX "~ghost/\">Ghostscript and Ghostview</A> for viewing and \n";
    print INDEX "printing the Postscript version.\n";
}
print INDEX "<HR>\n";
print INDEX "<p>Or you may view the GIF version in your browser:\n";
print INDEX "<CENTER>\n";
print INDEX "<TABLE BORDER=2 CELLSPACING=4 CELLPADDING=10 ALIGN=CENTER>\n";


$i=1;
open(PPM,"ls $tmpdir$base.*.ppm |");
@ppmfiles = <PPM>;
close(PPM);
$N = $#ppmfiles;

for ($j=0,$i=1; $j <= $N; ++$j) {
    $ppmfile = "$tmpdir$base." . $i . ".ppm";
    $gif_file = "$base\_$i.gif";
    system "ppmquant 256 $ppmfile | $xtra_cmds ppmtogif > $destdir$gif_file";
    system("rm $ppmfile \n");
    open(WRAP,"| cat > $destdir$base\_$i.html");
    print WRAP "<HTML>\n<HEAD>\n   <TITLE>$title (page $i)</TITLE>\n</HEAD>\n";
    print WRAP "<BODY>\n<CENTER><TABLE WIDTH=100%><TR>\n";
    if ($j > 0) {
        $p = $i-1;
        print WRAP "<TD WIDTH=33%><CENTER><A HREF=\"$base\_$p.html\">";
        print WRAP "Page $p";
#        print WRAP "Page $p<IMG SRC=\"$graphics_dir/back.gif\">";
        print WRAP "</A></CENTER></TD>\n";
    } else {
        print WRAP "<TD WIDTH=33%>&nbsp</TD>";
    }
    print WRAP "<TD WIDTH=33%><CENTER><A HREF=\"$base.html\">";
    print WRAP "$title</A>";
#    print WRAP "<IMG SRC=\"$graphics_dir/up.gif\"><BR>$title</A>";
    print WRAP "</CENTER></TD>\n";
    if ($j < $#ppmfiles) {
        $n = $i+1;
        print WRAP "<TD WIDTH=33%><CENTER><A HREF=\"$base\_$n.html\">";
        print WRAP "Page $n";
#        print WRAP "<IMG SRC=\"$graphics_dir/forward.gif\">Page $n";
        print WRAP "</A></CENTER></TD>\n";
    } else {
        print WRAP "<TD WIDTH=33%>&nbsp</TD>";
    }
    print WRAP "</TR></TABLE>\n";
    print WRAP "<P><A HREF=\"$gif_file\"><IMG SRC=\"$gif_file\"></A>";
    print WRAP "</P>\n";
    print WRAP "<CENTER><TABLE WIDTH=100%><TR>\n";
    if ($j > 0) {
        $p = $i-1;
        print WRAP "<TD WIDTH=33%><CENTER><A HREF=\"$base\_$p.html\">";
        print WRAP "Page $p";
#        print WRAP "Page $p<IMG SRC=\"$graphics_dir/back.gif\">";
        print WRAP "</A></CENTER></TD>\n";
    } else {
        print WRAP "<TD WIDTH=33%>&nbsp</TD>";
    }
    print WRAP "<TD WIDTH=33%><CENTER><A HREF=\"$base.html\">";
    print WRAP "$title</A>";
#    print WRAP "<IMG SRC=\"$graphics_dir/up.gif\"><BR>$title</A>";
    print WRAP "</CENTER></TD>\n";
    if ($j < $#ppmfiles) {
        $n = $i+1;
        print WRAP "<TD WIDTH=33%><CENTER><A HREF=\"$base\_$n.html\">";
        print WRAP "Page $n";
#        print WRAP "<IMG SRC=\"$graphics_dir/forward.gif\">Page $n";
        print WRAP "</A></CENTER></TD>\n";
    } else {
        print WRAP "<TD WIDTH=33%>&nbsp</TD>";
    }
    print WRAP "</TR></TABLE>\n";
    print WRAP "</CENTER>\n</BODY>\n</HTML>\n";
    close(WRAP);
    unless ($i-1 & 3)
    {
        if ($i > 0) {
	    print INDEX "</TR>";
	}
	print INDEX "<TR>\n";
    }
    print INDEX "<TD><A HREF=\"$base\_$i.html\">Page $i</A></TD>\n";
    $i++;
}
if ($i > 4) {
  while ($i-1&3) {
    $i++;
    print INDEX "<TD>&nbsp</TD>\n";
  }
}
close(PPM);
print INDEX "</TR></TABLE></CENTER><HR>\n";
print INDEX "<CENTER><font size=-3>created by";
print INDEX " <A HREF=\"http://www.ece.gatech.edu/users/dva/ps2www.html\">";
print INDEX "ps2www</A></font></CENTER></BODY>\n</HTML>\n";
close(INDEX);


