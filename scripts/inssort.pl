#!/opt/star/bin/perl -w

#
# Written J.lauret Mar 27 2001
# Sort Insure++ compilation warning list and output an html
# Arg1 : input file (mandatory)
# Arg2 : html file for output (optional ; default is STDOUT)
#
#
use lib "/afs/rhic.bnl.gov/star/packages/scripts";
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
    open(FO,">$OFILE") || die "Could not open $OFILE for write.\n";
    $FO    = FO;
} else {
    $OFILE = "";
    $FO    = STDOUT;
}

open(FI,$IFILE) || die "Could not open $IFILE\n";
while( defined($line = <FI>) ){
    if( $line =~ m/(\[.*\])(\s\*\*)(.*)(\*\*)/ ){
	# A new result is starting
	$code = $1;
	$error= $3;
	$ref  = "$code $error";
	#print "$1 $3\n";
	$CODE{$ref} = "";
    } elsif ( $line =~ m/(\[.*\])(\sError\:)(.*)/ ){
	$code = $1;
	$error= "Error";
	$ref  = "$code $error";
	$CODE{$ref} = $3;
    } elsif ( $line =~ m/(\[.*\])(\sWarning\:)(.*)/ ){
	$code = $1;
	$error= "Warning";
	$ref  = "$code $error";
	$CODE{$ref} = $3;
    } else {
	$CODE{$ref} .= $line;
    }
}
close(FI);


# Output HTML
print $FO IUhead("Insure++ compilation results");


print $FO "<H2>Summary</H2> (click for detail)\n<OL>\n";
foreach $line (sort keys %CODE){
    @items = split(" ",$line);
    $ref = IUGetRef($items[0]);
    print $FO "<LI><A HREF=\"#$ref\">$line</A>\n";
}
print $FO "</OL>";


print $FO "<H2>Details</H2>\n<OL>\n";
foreach $line (sort keys %CODE){
    @items = split(" ",$line);
    $CODE{$line} = IUl2pre($CODE{$line});
    $ref    = IUGetRef($items[0]);
    $docref = IUErrorURL($items[1],1);
    print $FO 
	"<LI><A NAME=\"$ref\"><B>$line</B></A>",$docref,"\n",
	"<PRE>$CODE{$line}</PRE>\n";
}
print $FO "</OL>\n".IUtrail();

# Be clean
if($OFILE ne ""){ close($FO);}




