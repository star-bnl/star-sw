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
$flag  = 1;

# Output HTML on the fly

while( defined($line = <FI>) ){
    chomp($line);

    if ($line =~ m/(VG_\()(.*)(\):)(.*)/){
	do {
	    push(@FATAL,$line);
	} while (defined($line = <FI> ) );

    } elsif ($line =~ m/(==\d+==)(.*)/){
	# a valgrind message was found
	$problem = $2;
	$problem =~ s/^\s*(.*?)\s*$/$1/;
	$routine = "";

	if ($problem eq ""){ 
	    $flag = 1;
	    #print "DEBUG $flag -- Found empty line (start new block)\n";
	    next;

	} elsif ($flag){
	    # first line of a new block
	    $rfcnt++;
	    $type   = $problem;
	    $flag   = 0;
	    if( defined($TYPE{$type}) ){
		$TYPE{$type} .= "$rfcnt ";
	    } else {
		$TYPE{$type} = "$rfcnt ";
	    }
	    $LINES[$rfcnt]  = $type;
	    $REASON[$rfcnt] = $type;
	    $GUILTY[$rfcnt] = "";    # don't know yet
	    $DUMP[$rfcnt]   = "";    # what is this ****
	    next;

	} else {
	    # the next line is the guilty routine in the stack
	    if ($GUILTY[$rfcnt] eq ""){
		if ($problem =~ /(at .*: )(.*\()(.*)(\))/){
		    # we know what it is
		    $functor = $2;
		    $routine = $3;
		    if ($routine =~ m/(.*)(:)(\d+)/){
			$routine = $1;
			$line    = $3;
		    } else {
			$line    = "?";
		    }
		    chop($functor);
		    $functor =~ s/^\s*(.*?)\s*$/$1/;

		    if   (($tmp = IUFuncMatch($routine)) ne "" ){  $routine = $tmp;}
		    else {                                         $routine = $functor;}

		    $GUILTY[$rfcnt] = IUl2pre($routine);
		    $REFS[$rfcnt]   = "<TT>$routine</TT>";
		    $LINES[$rfcnt]  = "line $line";
		} else {
		    # save as is (don't know)
		    $functor = $routine = "";
		    $line    = 0;
		    $GUILTY[$rfcnt] = "[".IUl2pre($problem)."]";
		    $REFS[$rfcnt]   = "";
		}

		# for any line like this, we know what it is
		if( $routine ne ""){
		    $routine = IUCanonicalName($routine);
		    if( defined($FUNC{$routine}) ){
			$FUNC{$routine} .= "$rfcnt ";
		    } else {
			$FUNC{$routine} = "$rfcnt ";
		    }
		}
	    } else {
		# Build an extended list of guilty ones
		if ( $problem =~ m/(by .*: )(.*)(\()(.*:\d+)(\))/) {
		    $routine = IUCanonicalName($4);
		    if( IUFuncMatch($DUMP[$rfcnt]) eq ""){
			$FUNC{$routine} .= "$rfcnt ";
		    } 
		}
	    }
	    
	    $DUMP[$rfcnt] .= "$problem\n";
	}
    }

}


#
# Ready to format now.
#

print $FO IUhead("Valgrind run-time messages");

@MENU=
    ("Problems by routines",
     "Problems by type",
     "Detailed valgrind dumps");


# dump menu with auto-references
print $FO "<UL>\n";
for ($k=0 ; $k <= $#MENU ; $k++){
    print $FO "<LI><A HREF=\"\#Menu$k\">$MENU[$k]</A>\n";
}
print $FO "</UL>\n";
$k = -1;

if ($#FATAL != -1){
    print $FO 
	"<H2>Untreated parsing</H2>\n",
	"A fatal error occured and baborted parsing. ",
	"This job is incomplete.\n",
	"<BR>\n",
	"<PRE>\n";

    foreach  $line (@FATAL){
	chomp($line);
	print $FO "$line\n";
    }
    print $FO "</PRE>\n";
}

# By routine
$k++;
print $FO "<A NAME=\"Menu$k\"></A><H2>$MENU[$k]</H2>\n<UL>\n";
foreach $routine (sort keys %FUNC){
    if (IUListRoutine($routine)){
	print $FO "<LI><B>$routine</B>\n";
	print $FO "<OL>\n";
	@items = split(" ",$FUNC{$routine});
	undef(%KNOWN);
	foreach $rfcnt (@items){
	    # need to do an extra check in valgrind mode i.e. errors
	    # are reported multiple times unlike Insure
	    if ( defined($KNOWN{$GUILTY[$rfcnt]." ".$LINES[$rfcnt]}) ){ next;}
	    $KNOWN{$GUILTY[$rfcnt]." ".$LINES[$rfcnt]} = 1;
	    #
	    # can add references like this later
	    # /webdatanfs/dox/html/StEvent_8h-source.html#l00153
	    #
	    print $FO 
		"<LI><A HREF=\"\#Ref$rfcnt\"><TT>$GUILTY[$rfcnt]</TT></A>",
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
    print $FO "<LI><FONT COLOR=\"#FF00000\">$type</FONT>\n";
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
	"<I>".IUl2pre($REASON[$rfcnt])."</I>\n",
	($REFS[$rfcnt] eq "" ? "":"in ".$REFS[$rfcnt]."<BR>"),
	#$REFS[$rfcnt]." $LINES[$rfcnt]<BR>",
	#"<TT>$GUILTY[$rfcnt]</TT><BR>",
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
