#!/usr/local/bin/perl
#
# $Id: lastBulletin.pl,v 1.5 1999/09/16 02:08:02 wenaus Exp $
#
# $Log: lastBulletin.pl,v $
# Revision 1.5  1999/09/16 02:08:02  wenaus
# Use RCF NFS based data area
#
# Revision 1.4  1999/08/17 22:01:44  wenaus
# Add folklore forum
#
# Revision 1.3  1999/08/17 02:55:03  wenaus
# eliminate reporting of who posted
#
# Revision 1.2  1999/08/14 15:25:28  wenaus
# suppress cmd output
#
# Revision 1.1  1999/08/14 15:00:18  wenaus
# Reports time since last software bulletin
#
#
######################################################################
#
# lastBulletin.pl
#
# Torre Wenaus 8/99
#
# Report time since last software bulletin, last ROOT folklore
# posting...
#
# To be run from the machine hosting HyperNews (duvall)
#

use Time::Local;

$curTime = time();
$poster = '';
$hours = '';
$delHrs = 0;

&getLast("/usr/local/apache/htdocs/hn/bulletin.html,urc","lastBulletin");
&getLast("/usr/local/apache/htdocs/hn/folklore.html,urc","lastFolklore");
exit;

sub getLast {
    my ( $fname, $ofile ) = @_;
    open(FILE,"<$fname");
    while (<FILE>) {
        if ( m/^LastMod/ ) {
            ( $tag, $wk, $dy, $cmo, $yr, $hhmmss, $zn ) = split / /;
        } elsif ( m/^From/ ) {
            ( $tag, $poster ) = split / /;
            $poster =~ s/\s//g;
        }
    }
    close(FILE);
    $hhmmss =~ m/(\d\d):(\d\d):(\d\d)/;
    $hh = $1;
    $mm = $2;
    $ss = $3;
    %cmos = (
             'Jan' => 1,
             'Feb' => 2,
             'Mar' => 3,
             'Apr' => 4,
             'May' => 5,
             'Jun' => 6,
             'Jul' => 7,
             'Aug' => 8,
             'Sep' => 9,
             'Oct' => 10,
             'Nov' => 11,
             'Dec' =>12
             );
    $mo = $cmos{$cmo};
    
    $modTime = timegm($ss, $mm, $hh, $dy, $mo-1, $yr-1900);
    $delTime = $curTime - $modTime;
    $delHrs = $delTime/3600; # hours
    $hours = 'hrs';
    $chours = sprintf("%d",$delHrs);
    if ( $chours < 2 ) {$hours = 'hr'}
    if ( $chours > 48 ) {
        $delHrs = $delHrs /24;
        $hours = 'days';
    }
    if ( $poster ne '' ) {
        $who = "($poster)";
    } else {
        $who = '';
    }

    $floc = "/star/starlib/doc/www/html/comp-nfs";
    open(OFILE,">$floc/$ofile.txt") or die "File open failure $!";
    $oline = sprintf("Last posting %d $hours\n",$delHrs);
    print OFILE $oline;
    close OFILE;
    $cmd =
        "rm -f $floc/$ofile.pbm $floc/$ofile.ppm $floc/$ofile.gif;"
            ."pbmtext '$oline' > $floc/$ofile.pbm;"
                ."pnmcrop $floc/$ofile.pbm > $floc/$ofile.ppm 2>/dev/null;"
                    ."ppmtogif -transparent rgb:ffff/ffff/ffff $floc/$ofile.ppm > $floc/$ofile.gif 2>/dev/null";
    $result = system($cmd);
}

