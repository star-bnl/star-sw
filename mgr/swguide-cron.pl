#!/opt/star/bin/perl
#
# $Id: swguide-cron.pl,v 1.1 1999/07/07 13:21:07 wenaus Exp $
#
# $Log: swguide-cron.pl,v $
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

$fpath = "/usr/local/apache/htdocs/code";
$pgm = "/star/u2d/wenaus/datadb/swguide.pl";
if ( @ARGV ) {
    @ver = @ARGV;
} else {
    @ver = ( "dev", ".dev", "new", "pro", "old" );
}

print "Build SW guide for versions";
foreach $v ( @ver ) {
    print " '$v'";
}
print "\n";

@detail = ( 0, 1, 2 );

foreach $v ( @ver ) {
    foreach $d ( @detail ) {
        $fname = $fpath."/swguide-$v-$d.html";
        if ( -e $fname ) { unlink($fname) or die "Can't delete $fname: $!\n"; }
        open(FILE,">$fname") or die "Can't write to $fname: $!\n";
        print "$v-$d\n";
        $command = "$pgm ver=$v detail=$d dynamic=yes";
        $output = `$command`;
        print FILE $output;
        close (FILE);
    }
}
