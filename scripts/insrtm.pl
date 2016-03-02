#!/usr/local/bin/perl -w

# Written J.lauret Mar 27 2001
# This scripts take an input file as sole argument
# and output result in HTML format to STDOUT
#
use lib "/afs/rhic.bnl.gov/star/packages/scripts/";
use ABUtils;


if( defined($ARGV[0]) ){
    $IFILE = $ARGV[0];
} else {
    print "Input file : ";
    chomp($IFILE = <STDIN>);
}
if( ! -e $IFILE){ die "Could not find $IFILE\n";}

if( defined($ARGV[1]) ){
    $OFILE = $ARGV[1];
    open(FO,">$OFILE-tmp") || die "Could not open $OFILE-tmp\n";
    $FO    = FO;
} else {
    $FO    = STDOUT;
}



open(FI,$IFILE) || die "Could not open $IFILE\n";

$rfcnt = -1;     # reference count
# Insure dumps lots of messages at the end we do not need
# This string is the limit at which we have to stop.
$stop  = "Leaks detected during execution";
$flag  = 0;

# Output HTML on the fly

while( defined($line = <FI>) ){
    chomp($line);

    # quit when this is found
    if( $line =~ m/$stop/ ){ last;}
   
    # An error comes in this flavor
    if( $line =~ m/(.*)(\*\*)([\w\(\)\d]+)(\*\*)/){
	$rfcnt++;

	$routine = $1;
	$type    = $3;
	# Build reference list for this kind of errors
	if( defined($TYPE{$type}) ){
	    $TYPE{$type} .= "$rfcnt ";
	} else {
	    $TYPE{$type} = "$rfcnt ";
	}
	$LINES[$rfcnt]  = "?";
	$GUILTY[$rfcnt] = "";
	$REASON[$rfcnt] = "";
	$DUMP[$rfcnt]   = "";

	# build reference list for this routine
	if($routine ne ""){
	    $routine =~ m/(\[)(.*):(\d+)\]/;
	    $routine = $2;
	    $linenum = $3;
	    #print "$routine $linenum\n";
	    if( defined($FUNC{$routine}) ){
		$FUNC{$routine} .= "$rfcnt ";
	    } else {
		$FUNC{$routine} = "$rfcnt ";
	    }
	    $REFS[$rfcnt] = "<TT>$routine</TT>";
	    $LINES[$rfcnt]= "line $linenum <B>".IUErrorURL($type)."</B>";
	} else {
	    $REFS[$rfcnt] = "<i>No function name info</i>";
	}
    } else {
	if($rfcnt == -1){ next;}

	if ($line =~ m/(>>\s+)(.*)/){
	    # this line comes after the error type.
	    # It indicates the line and error.
	    # Note that we DO have the line number from the
	    # preceeding block and we assume that this block
	    # cannot be executed if the preceeding was not.
	    $GUILTY[$rfcnt] = IUl2pre($2);

	    # we set this flag so the next line(s) will be
	    # considered as the reason for this error.
	    $flag = 1;

	} else {
	    # remaining are
	    #  - the reason line
	    #  - the core dump
	    # all are attached to $rfcnt
	    if($line ne "" && $flag == 1){
		$REASON[$rfcnt] = "$line ";
		$flag++;
	    } else {
		# $line eq "" or $flag > 1
		if($line eq "" && $flag != 1){  
		    # reset the flag. Block 3+
		    $flag = 0;
		} elsif ($flag == 2){
		    # block 2, grab the reason for that
		    # error.
		    $REASON[$rfcnt] .= "$line ";
		} else {
		    $DUMP[$rfcnt] .= "$line\n";
		}
	    }
	}
    }
}


#
# Ready to format now.
#

print $FO IUhead("Insure++ run-time messages");

@MENU=
    ("Problems by routines",
     "Problems by type",
     "Detailed Insure dump");

# dump menu with auto-references
print $FO "<UL>\n";
for ($k=0 ; $k <= $#MENU ; $k++){
    print $FO "<LI><A HREF=\"\#Menu$k\">$MENU[$k]</A>\n";
}
print $FO "</UL>\n";
$k = -1;


# By routine
$k++;
print $FO "<A NAME=\"Menu$k\"></A><H2>$MENU[$k]</H2>\n<UL>\n";
foreach $routine (sort keys %FUNC){
    if (IUListRoutine($routine)){
	print $FO "<LI><B>$routine</B>\n";
	print $FO "<OL>\n";
	@items = split(" ",$FUNC{$routine});
	foreach $rfcnt (@items){
	    print $FO 
		"<LI><A HREF=\"\#Ref$rfcnt\"><TT>$GUILTY[$rfcnt]</TT></A><BR>\n",
		"    $LINES[$rfcnt] <BR>",
		"    <FONT COLOR=\"#EE0000\"><I>".IUl2pre($REASON[$rfcnt])."</I></FONT>\n";
	}
	print $FO "</OL><P>\n";
    }
}
print $FO "</UL>\n";


# By type
$k++;
print $FO "<A NAME=\"Menu$k\"></A><H2>$MENU[$k]</H2>\n<UL>\n";
foreach $type (sort keys %TYPE){
    print $FO "<LI><B>".IUErrorURL($type)."</B>\n";
    print $FO "<OL>\n";
    @items = split(" ",$TYPE{$type});
    $prcdt = "";
    foreach $rfcnt (@items){
	$current = $REFS[$rfcnt];
	if (IUListRoutine($current)){
	    if($current ne $prcdt ){
		print $FO "<LI>$current ";
		$prcdt = $current;
	    }
	    print $FO "[<A HREF=\"\#Ref$rfcnt\">$rfcnt</A>]&nbsp;";
	}
    }
    print $FO "</OL><P>\n";
}
print $FO "</UL>\n";



# Most comprehensive list
$k++;
print $FO "<A NAME=\"Menu$k\"></A><H2>$MENU[$k]</H2>\n<UL>\n";
for ($i=0 ; $i <= $#REFS ; $i++){
    $rfcnt = $i;
    print $FO 
	"<LI><A NAME=\"Ref$rfcnt\"></A>",
	$REFS[$rfcnt]." $LINES[$rfcnt]<BR>",
	"<TT>$GUILTY[$rfcnt]</TT><BR>",
	"<FONT COLOR=\"#EE0000\"><I>".IUl2pre($REASON[$rfcnt])."</I></FONT><BR>\n",
	"<PRE>\n".IUl2pre($DUMP[$rfcnt])."</PRE>\n";
	
}
print $FO "</UL>\n";



# The /UL is here terminated in the line patterns
print $FO IUtrail();
if($FO ne STDOUT){
    close($FO);
    if(-e "$OFILE"){ unlink("$OFILE");}
    rename("$OFILE-tmp","$OFILE");
}
