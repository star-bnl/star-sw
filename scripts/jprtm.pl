#!/usr/local/bin/perl -w

# Written as interface to JProf formatting
# However, arguments are expected to be the same
# i.e. arg1 program
#      arg2 jprof-log
#
use lib "/afs/rhic.bnl.gov/star/packages/scripts/";
use ABUtils;
    

$xprgm = shift(@ARGV) if (@ARGV);
$fprof = shift(@ARGV) if (@ARGV);


# Check arguments
if( ! -e $xprgm ){
    print "First argument must be a program name\n";
    print "Received ".join(" ",@ARGV)."\n";
    exit;
} elsif ( ! -e $fprof){
    print "Second argument must be a profiling file\n";
    print "Received ".join(" ",@ARGV)."\n";
    exit;
} 


# Sort out path/file-name
if( $fprof =~ m/(.*\/)(.*)/ ){
    $path = $1; $file = $2;
} else {
    $path = "."; $file = $fprof;
}


# Dup STDERR (we don't care about the error)
open(STDERR,">/dev/null");
$jprof = JPRExec();
if( ! -e $jprof){
    print "Could not locate the jprof program\n";
    exit;
}


# get the result in an array
@all = `cd $path && $jprof $xprgm $file`;
$BODY = IUbody();


# And format it out
if($#all == -1){
    print IUhead("Profiling result could not be extracted.");
    print "Chain crashed\n";
    print IUtrail();
} else {
    foreach $line (@all){
	if( $line =~ m/<body>/i){      
	    $line =~ s/(.*)(<body>)(.*)/$1$BODY$3/;
	} elsif ($line =~ m/<h1>.*<\/h1>/){ 
	    $line .= "\n<h5 align=\"center\">Created on ".localtime()."</h5>\n";
	}
	print $line;
    }
}


