#!/usr/local/bin/perl -w

#
# Written J.Lauret sometime in January 2002
# 2013 modified for additional menues
#
# Reads BFChain.cxx and spit out an HTML formatted
# table of related-options (clickable).
#
# Planned : Add an option 'expand' (i.e. detail
#           what an option really means as per basic
#           maker, order they run etc ...). Will need
#           and extra threading.
#

$IN = shift(@ARGV) if (@ARGV);
$OUT= shift(@ARGV) if (@ARGV);

$MAXCHAINOPT = 2;   # maximum number of blocks for chain definition
$REL         = "."; # relative path to codes

if ( -d "StRoot"){     $REL = "StRoot/StBFChain";}
if ( -d "StBFChain"){  $REL = "StBFChain";}

if( ! defined($IN) ){  $IN  = "$REL/StBFChain.cxx";}
if( ! defined($OUT)){  $OUT = "$REL/doc/index.html";}

if ( -e "$REL/doc/BFC.h" && -e "$REL/doc/BFC2.h"){
    # Do the merging on the fly and parse the merged file
    print "New mode (separate includes)\n";
    open(SRC, "$REL/StBFChain.cxx")         || die "Cannot open $REL/StBFChain.cxx\n";
    open(FIN,">$REL/doc/StBFChain.cxx_doc") || die "Cannot open $REL/doc/StBFChain.cxx_doc\n";
    while ( defined($line = <SRC>) ){
	chomp($line);
	if ($line =~ m/^(\/\/.include\s*\")(.*)(\"\s*)/) {
	    if ( -e "$REL/doc/$2" && $2 ne "StBFChain.h"){   # Any found include but not the class def
		print "\tAdding $REL/doc/$2 --> [$line] [$2]\n";
		open(INT,"$REL/doc/$2");
		while( defined($line = <INT>) ){ print FIN $line;}
		close(INT);
		next;
	    }
	}
	print FIN "$line\n";
    }
    close(SRC);
    close(FIN);

    $IN = "$REL/doc/StBFChain.cxx_doc";
} else {
    print "All in source mode\n";
}

print "$0 $IN $OUT\n";


if ( ! open(FI,"$IN") ){ die "Could not open $IN for reading\n";}

if ($0 !~ m/^\//){
    chomp($SELF = `/bin/pwd`);
    $SELF = "$SELF/$0";
} else {
    $SELF = $0;
}
$SSELF = $SELF; $SSELF =~ s/.*\///;

$parse = 0;
while ( defined($line = <FI>) ){
    if($line =~ /Bfc_st BFC/){
	$savel = 1;
	# those would work only with a full doc/StBFChain.cxx_doc not pre-generated 
	# or with a pre-processor command not removing commments 
	if ($line =~ m/(Bfc_st BFC.*)(\/\/)(.*)/){
	    $TITLE[$parse] = $3;
	} else {
	    # so, this default is more likely
	    $TITLE[$parse] = "Block $parse";
	}
	print "DEBUG:: Saving $parse $TITLE[$parse]\n";
	$parse++;
    } elsif ($line =~ m/NoChainOptions/){
	last;
    } elsif ($line =~ /\};/ ){
	if ($parse >= 2){   last;}
	$savel = 0;
    } elsif ($savel){
	$line =~ s/\/\/.*//;
	$line =~ s/^\s*(.*?)\s*$/$1/;
	if ($parse == 1){
	    push(@lines1,$line) ;#if ($line ne "");
	} else {
	    push(@lines2,$line) ;#if ($line ne "");	    
	}
    }
}
close(FI);

$ckk=0;
do {
    # Use either arrays
    if ($ckk == 0){ 
	push(@lines,@lines1);
    } else {
	push(@lines,@lines2);
    }

    # Clean up Global arrays wich will be used later
    undef(%COLOR);
    undef(%OKEY);
    undef(%KNAME);
    undef(%KCHAIN);
    undef(%KOPT);
    undef(%KMAKE);
    undef(%KLIBS);
    undef(%KCMT);
    undef(@KEYS);
    undef(@items);

    for ($i=0 ; $i <= $#lines ; $i++){
	chomp($line  = $lines[$i]);

	# advance to form a full line 
	while ($line !~ /\}/ && $i != $#lines){
	    #print "[$line]\n";
	    $i++;
	    $line .= $lines[$i]; # keep concatenating until terminated
	    chomp($line);        # may span multiple lines
	}
	push(@items,split(/\"\s*,/,$line)); # not that the STR_OBSOLETE lines will not show up

	$flag = 1;
	for($j=0 ; $j <= $#items ; $j++){
	    $items[$j] =~ s/[\{\"\}]//g;
	    $items[$j] =~ s/,/ /g;
	    if( index($items[$j],"---") != -1){
		$items[$j] =~ s/-/ /g;
	    }
	    $items[$j] =~ s/^\s*(.*?)\s*$/$1/;
	    
	    # any line which is empty apart from the first element
	    # will be flagged - this will allow for subtitle to appear
	    if( $j != 0 && $j != $#items){
		#print STDERR "$flag [$items[$j]] ";
		$flag = $flag & ($items[$j] eq "");
		if($items[$j] eq ""){ $items[$j] = "&nbsp;"}
	    }
	}
    
	# Now we are ready
	#$menu= 
	$key = uc($items[0]);
	$key =~ s/\s+/_/g;

	if( $i == 0){
	    $COLOR{$key} = "orange";
	} elsif( $flag ){
	    $COLOR{$key} = "cornsilk";
	} else {
	    $COLOR{$key} = "";
	}


	push(@KEYS,$key);
	$OKEY{$key}   = $items[0];
	$KNAME{$key}  = $items[1];
	$KCHAIN{$key} = $items[2];
	$KOPT{$key}   = $items[3];
	$KMAKE{$key}  = $items[4];
	$KLIBS{$key}  = $items[5];
	$KCMT{$key}   = $items[6];

	undef(@items);
    }
    undef(@lines);

    # if ($ckk == 0){
    #push(@LINES,"%%MENU%%");

	#"<UL>\n");
	#for($ii=0 ; $ii < 2 ; $ii++){
	#    print FO "<LI><A HREF=\"#".&GetRef($TITLE[$ii])."\">$TITLE[$ii]</A>\n";
	#}
	#print FO "</UL>\n<P><HR>";
    #}

    # keep sub-meny references
    $REFS{$ckk} = "";

    push(@LINES,
	 "<H2><A NAME=\"".&GetRef($TITLE[$ckk])."\">Options for $TITLE[$ckk]</A></H2>\n",
	 "<table border=\"1\">\n");

    foreach $key (@KEYS){
	if( $key eq ""){ next;}
	if( ! defined($COLOR{$key}) ){  
	    $COLOR{$key} = "";
	    print STDERR "Missing color for key=$key\n";
	}
	if( $COLOR{$key} ne "" ){
	    $col = " BGCOLOR=\"$COLOR{$key}\"";
	} else {
	    $col = "";
	}

	$test = join(";",($KCHAIN{$key},$KNAME{$key},$KMAKE{$key},$KLIBS{$key},$KCMT{$key},$KOPT{$key}));
	$test =~ s/[nbsp;\& ]//g;

	if ( $test eq ""){
	    $OKEY{$key} =~ s/[\|;]/ /g;  # eliminate those characters as separators
	    push(@LINES,"<tr$col><td colspan=\"7\"><A NAME=\"$key$ckk\">$OKEY{$key}</A></td></tr>\n");
	    $REFS{$ckk} .= "$OKEY{$key};$key$ckk|";
	} else {
	    push(@LINES,"<tr$col><td><A NAME=\"$key$ckk\">$OKEY{$key}</A></td><td>$KNAME{$key}</td><td>$KCHAIN{$key}</td>");

	    push(@LINES,"<td>");	
	    @items = split(" ",$KOPT{$key});
	    foreach $el (@items){
		$tmp = uc($el);
		if( defined($KNAME{$tmp}) ){
		    push(@LINES,"<A HREF=\"\#$tmp$ckk\">$el</A> ");
		} else {
		    push(@LINES,"$el ");
		}
	    }
	    push(@LINES,"</td><td>$KMAKE{$key}</td><td>$KLIBS{$key}</td><td>$KCMT{$key}</td></tr>\n");
	}
	
    }
    push(@LINES,"</table>\n");
    $ckk++;
} while($ckk < $MAXCHAINOPT);



if ( !open(FO,">$OUT") ){ 
    die "Could not open $OUT for writing\n";

} else {
    print FO
	"<head><title>BFChain Options</title></head>\n",
	"<html>\n",
	"<body bgcolor=white>\n",
	"<!-- Generated by $SELF @ARGV -->\n",
	"<H1 ALIGN=\"center\">BFChain Options</H1>\n";

    # loop over the N blocks
    for($ii=0 ; $ii <= 1 ; $ii++){
	print FO "<LI><A HREF=\"#".&GetRef($TITLE[$ii])."\">$TITLE[$ii]</A>\n";
	@items = split(/\|/,$REFS{$ii});
	if ($#items != 0){
	    print FO "\t<UL>\n";
	    foreach $el (@items){
		($name,$tag) = split(";",$el);
		print FO "\t<LI><A HREF=\"\#$tag\">$name</LI>\n";
	    }
	    print FO "\t</UL>\n";
	}
    }
    print FO "</UL>\n<P><HR>";

    foreach $line (@LINES){ print FO $line;}
    
    print FO  
	"<h5>Created by $SSELF on ".localtime()." - J. Lauret</h5>\n",
	"</body>\n",
	"</html>\n";
    close(FO);
}


# Returns a URL ref
sub GetRef
{
    my($arg)=@_;
    $arg =~ s/[ !]/_/g;
    $arg;
}
