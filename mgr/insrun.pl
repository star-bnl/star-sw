#!/usr/local/bin/perl

#
# Written J.lauret Mar 27 2001
# This scripts take an input file as sole argument
# and output result in HTML format to STDOUT
#
use lib "/afs/rhic/star/packages/DEV/mgr";
use InsUtils;


if( defined($ARGV[0]) ){
    $IFILE = $ARGV[0];
} else {
    print "Input file : ";
    chomp($IFILE = <STDIN>);
}
if( ! -e $IFILE){ die "Could not find $IFILE\n";}


open(FI,$IFILE) || die "Could not open $IFILE\n";

$st  = "Leaks detected during execution";
$mn  = 0;     # meaning of the error
$rtnm= 1==0;  # get routine name
$smr = 0;     # add summary title


# Output HTML on the fly
print IUhead("Insure++ run-time messages");


$rfcnt=0;
while( defined($line = <FI>) ){
    chomp($line);

    if( $line =~ m/$st/ ){ 
	last;
    }

    $line = IUl2pre($line);

    if($mn > 0 && $line ne ""){ 
	@items = split(":",$line);
	$error = shift(@items);
	$line  = join(":",@items);
	$line  = "$error : <TT>$line</TT>\n<PRE>";
	$mn    = -1;
	# After the pre, the first "::" detected is the routine name
	# and reference. So, we will keep an extra variable reset here
	# for the routine recognition and will use the $title later on
	# as well.
	$rtnm  = 1==1;
    }
    if( $rtnm && $line =~ m/::/){
	$rtnm = 1==0;
	$tmp  = $line;
	$tmp  =~ s/\s//g;
	if( defined($REF{$tmp}) ){
	    $REF{$tmp} .= "<LI><A HREF=\"#Refer$rfcnt\">$error</A>\n";
	} else {
	    $REF{$tmp}  = "<LI><A HREF=\"#Refer$rfcnt\">$error</A>\n";
	}
    }
    if( $line =~ m/(\*\*)([\w\(\)\d]+)(\*\*)/){
	$rfcnt++;
	if($mn == -1){ push(@ALL,"</PRE>\n");}
	$line = "<LI><A NAME=\"Refer$rfcnt\"></A><B>$2</B><BR>\n";
	$mn   = 1;
    }
    if($line =~ m/summary/i && $smr == 0){
	$smr++;
	push(@ALL,"</UL>\n</PRE><H2>Summary</H2><PRE>\n");
    }

    push(@ALL,"$line\n");
}


print 
    "<H2>Problems</H2> (list may be incomplete, see all errors below)",
    "<UL>\n";
foreach $line (sort keys %REF){
    print "<LI><TT><B>$line</B></TT><UL>$REF{$line}</UL>\n";
}
print "</UL>\n";

print 
    "<H2>All run-time errors</H2>",
    "<UL>\n";
foreach $line (@ALL){ 
    print $line;
}
# The /UL is here terminated in the line patterns
print IUtrail();



