#!/usr/bin/env perl
#
# $Id: swguide-cron.pl,v 1.8 2007/11/05 20:04:32 jeromel Exp $
#
# $Log: swguide-cron.pl,v $
# Revision 1.8  2007/11/05 20:04:32  jeromel
# Path change / cleanup
#
# Revision 1.7  2006/08/15 18:05:43  jeromel
# Bundle rhic -> rhic.bnl.gov
#
# Revision 1.6  2004/11/16 01:49:03  jeromel
# Modified /afs/rhic.bnl.gov/ to /afs/rhic.bnl.gov/
#
# Revision 1.5  2002/01/26 23:54:50  jeromel
# *** empty log message ***
#
# Revision 1.4  2002/01/26 23:34:00  jeromel
# Corrected and secured.
#
# Revision 1.3  2001/12/28 21:20:16  jeromel
# comp-nfs -> tmp
#
# Revision 1.2  2001/12/08 00:53:03  jeromel
# Added temp file mechanism for security.
#
# Revision 1.1  2001/11/21 20:24:42  jeromel
# Moved from dev/mgr to STAR_SCRIPTS.
#
# Revision 1.4  1999/09/20 22:55:17  wenaus
# Move output area to RCF NFS web area
#
# Revision 1.3  1999/07/25 16:27:30  wenaus
# Debug printout only
#
# Revision 1.2  1999/07/22 22:05:05  wenaus
# kill printout
#
# Revision 1.1  1999/07/07 13:21:07  wenaus
# faster and more info presented
#
#
######################################################################
#
# swguide-cron.pl
#
# T. Wenaus 6/99
#
# Build the SW guide static pages for each release
#
# Usage: swguide-cron.pl [dev .dev new pro old]
#

$fpath = "/afs/rhic.bnl.gov/star/doc/www/html/tmp";
if( defined($ENV{STAR_CGI}) ){
    $prgm = $ENV{STAR_CGI}."/swguide.pl";
} else {
    $prgm = "/afs/rhic.bnl.gov/star/packages/cgi"."/swguide.pl";
}
if ( @ARGV ) {
    @ver = @ARGV;
} else {
    @ver = ( "dev", ".dev", "new", "pro", "old" );
}

$debugOn = 0;
if ( $debugOn ) {
    print "Build SW guide for versions";
    foreach $v ( @ver ) {
        print " '$v'";
    }
    print "\n";
}

@detail = ( 0, 1, 2 );

foreach $v ( @ver ) {
    foreach $d ( @detail ) {
        $fname = $fpath."/swguide-$v-$d.html";
        if ( -e $fname ) { unlink($fname) or die "Can't delete $fname: $!\n"; }
        open(FILE,">$fname-tmp") or die "Can't write to $fname: $!\n";
        print "$v-$d\n" if $debugOn;
        $command = "$prgm ver=$v detail=$d dynamic=yes";
	print "Executing [$command]\n" if $debugOn; 
        $output = `$command`;
        print FILE $output;
        close (FILE);

	# Added for space security. Note that this script is in 
	# buffered IO mode.
	if( -e "$fname-tmp"){
	    @items = stat("$fname-tmp");
	    if($#items != -1){        # oh yeahhh. This can be true too ...
		if( $items[7] != 0){
		    rename("$fname-tmp","$fname");
		}
	    }
	}
    }
}
