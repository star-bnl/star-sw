#!/usr/local/bin/perl
#
# $Id: lastBulletin.pl,v 1.2 1999/08/14 15:25:28 wenaus Exp $
#
# $Log: lastBulletin.pl,v $
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
# Report time since last software bulletin
#

use Time::Local;

$curTime = time();
open(BFILE,"</usr/local/apache/htdocs/hn/bulletin.html,urc");
while (<BFILE>) {
    if ( m/^LastMod/ ) {
        ( $tag, $wk, $dy, $cmo, $yr, $hhmmss, $zn ) = split / /;
    } elsif ( m/^From/ ) {
        ( $tag, $poster ) = split / /;
        $poster =~ s/\s//g;
    }
}
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
$floc = "/star/datapool/web";
open(OFILE,">$floc/lastBulletin.txt") or die "File open failure $!";
$oline = sprintf("Last mod %d $hours $who\n",$delHrs);
print OFILE $oline;
close OFILE;
close BFILE;
$cmd =
    "rm -f $floc/lastBulletin.pbm $floc/lastBulletin.ppm $floc/lastBulletin.gif;"
    ."pbmtext '$oline' > $floc/lastBulletin.pbm;"
    ."pnmcrop $floc/lastBulletin.pbm > $floc/lastBulletin.ppm 2>/dev/null;"
    ."ppmtogif -transparent rgb:ffff/ffff/ffff $floc/lastBulletin.ppm > $floc/lastBulletin.gif 2>/dev/null";
$result = system($cmd);
