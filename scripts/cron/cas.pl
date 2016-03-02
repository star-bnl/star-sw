#!/opt/star/bin/perl
#
# $Id: cas.pl,v 1.4 2002/02/27 00:54:54 jeromel Exp $
#
# $Log: cas.pl,v $
# Revision 1.4  2002/02/27 00:54:54  jeromel
# Those are old scripts (probably obsolete) in which I have updated the
# output path to reflect the newer directory structure tree (comp-nfs
# business).
#
# Revision 1.3  1999/11/22 18:19:13  wenaus
# remove rmods03
#
# Revision 1.2  1999/10/07 15:21:11  wenaus
# Use wide ps output
#
# Revision 1.1  1999/09/16 16:32:28  wenaus
# cron script producing CAS usage summary
#
#
######################################################################
#
# cas.pl
#
# Author: T. Wenaus
#
# Builds a web page summarizing CAS usage. For use in a cron job
# from rcf.
#
# Usage: cas.pl in cron jobs
#

$htmlFile = '/star/starlib/doc/www/html/tmp/casusage.html';

if ( $ARGV[0] ne '' ) {
    $debugOn = 1;
} else {
    $debugOn = 0;
}
$host = ' ';
$host = `hostname`;
chomp $host;
if ( $host eq 'rcf.rhic.bnl.gov' ) {
    $ssh = '/usr/local/src/ssh-1.2.26/bin/ssh';
} else {
    $ssh = 'ssh';
}
open (FILE, ">$htmlFile") or die "File write failed";
$timenow = " ";
$timenow = localtime(time());
print FILE <<END;
<html>
<head>
<title>STAR CAS Usage</title>
</head>
<table border=0 width="100%" valign=top>
<tr valign=top><td align="left" valign=top><h3>STAR CAS usage</h3></td>
<td align="right" valign=top>Last updated $timenow</td></tr></table>

Local space, users with lots of accumulated CPU usage (or root4star users on Suns), and 1min load average
are reported. 'Server up' refers to a job submission and monitoring server
in development. 
An rcas load of ~2
indicates a loaded machine (they have two CPUs).
Local scratch space is at /home/scratch on rcas nodes.
CRS nodes currently available interactively and a few
Suns and STAR nodes are also shown. Please report problems
immediately to wenaus\@bnl.gov
<p>
<pre>
END
close (FILE);

%nodes = (
          'rcas0201.rcf.bnl.local' => '/dev/sda4',
          'rcas0202.rcf.bnl.local' => '/dev/sda4',
          'rcas0203.rcf.bnl.local' => '/dev/sda4',
          'rcas0204.rcf.bnl.local' => '/dev/sda4',
          'rcas0205.rcf.bnl.local' => '/dev/sda4',
          'rcas0206.rcf.bnl.local' => '/dev/sda4',
          'rcas0207.rcf.bnl.local' => '/dev/sda4',
          'rcas0208.rcf.bnl.local' => '/dev/sda4',
          'rcas0209.rcf.bnl.local' => '/dev/sda4',
          'rcas0210.rcf.bnl.local' => '/dev/sda4',
          'rcas0211.rcf.bnl.local' => '/dev/sda4',
          'rcas0212.rcf.bnl.local' => '/dev/sda4',
          'rcas0213.rcf.bnl.local' => '/dev/sda4',
          'rcas0214.rcf.bnl.local' => '/dev/sda4',
          'rcas0215.rcf.bnl.local' => '/dev/sda4',
          'rcas0216.rcf.bnl.local' => '/dev/sda4',
          'rcas0218.rcf.bnl.local' => '/dev/sda4',
          'rcas0219.rcf.bnl.local' => '/dev/sda4',
          'rcas0220.rcf.bnl.local' => '/dev/sda4',
          'rcas0221.rcf.bnl.local' => '/dev/sda4',
          'rcas0222.rcf.bnl.local' => '/dev/sda4',
          'rcas0223.rcf.bnl.local' => '/dev/sda4',
          'rcas0224.rcf.bnl.local' => '/dev/sda4',
          'rcas0225.rcf.bnl.local' => '/dev/sda4',
          'rcas0226.rcf.bnl.local' => '/dev/sda4',
          'rcas0227.rcf.bnl.local' => '/dev/sda4',
          'duvall.star.bnl.gov' => '/dev/hda3',
          'rsun00.rhic.bnl.gov' => 'Solaris',
          'sol.star.bnl.gov' => 'Solaris',
        );
#          'robinson.star.bnl.gov' => '/dev/rd/c0d0p1',

open (FILE, ">>$htmlFile");
printf FILE ("         Node            Free  Used Jobs Load Server    CPU intensive users\n");
close(FILE);
foreach $node ( sort keys %nodes ) {
    print "$node\n" if $debugOn;
    open (FILE, ">>$htmlFile");
    printf FILE ("%-22s",$node);
    $diskname = $nodes{$node};
    if ( $nodes{$node} eq 'Solaris' ) {
        $pscmd = 'ps -edaf;';
        $dfcmd = '';
        $srvcmd = '';
        $uscmd = '';
    } else {
        $pscmd = 'ps aux;';
        $pscmd = '';  # root4star check replaced with CPU usage check
        $dfcmd = "df -k $diskname;";
        $srvcmd = 'ps axw | grep perl | grep analysisServer;';
        $uscmd = "ps auxw | perl -e 'while (<STDIN>) {\@ln = split; \\\$tm = substr(\\\$_,53,2); if (\\\$tm>5 && \\\$ln[0] ne \\\"root\\\") {print \\\"\\\$ln[0] using root4star or lots of CPU\\n\\\"}}';";
    }
    $sshcmd = "$ssh -x $node \"$dfcmd $pscmd $srvcmd $uscmd uptime\"";
    @output = `$sshcmd`;
    print @output if $debugOn;
    $rootusers = '';
    $nr=0;
    $space = '--  ';
    $pct = '--';
    $load = 0;
    $srvUp = '    ';
    foreach $line ( @output ) {
      chomp $line;
      if ( $line =~ m|^$diskname| || $line =~ /[0-9]+%/ ) {
        print "$line\n" if $debugOn;
        @tokens = split /\s+/, $line;
        $space = $tokens[3];
        $pct = $tokens[4];
      }
      if ( $line =~ m/root4star/ ) {
        print "$line\n" if $debugOn;
        $line =~ m/^\s*([a-z0-9_]+)/;
        $rootusers .= $1." ";
        $nr++;
      }
      if ( $line =~ m/load average:\s+([0-9.]+)/ ) {
        $load = $1;
      }
      if ( $line =~ m/analysisServer/ ) {
        $srvUp = ' up ';
      }
    }
    $space = $space/1000;
    printf FILE ("%6dMB %4s %2d   %s %s %s\n",$space,$pct,$nr,$load,$srvUp,$rootusers);
    close(FILE);
}

open (FILE, ">>$htmlFile");
print FILE "</pre></html>\n";
close(FILE);
