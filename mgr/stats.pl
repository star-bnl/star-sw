#!/opt/star/bin/perl
#
# $Id: stats.pl,v 1.2 1999/09/21 12:18:14 wenaus Exp $
#
# $Log: stats.pl,v $
# Revision 1.2  1999/09/21 12:18:14  wenaus
# Data area moved to RCF NFS
#
# Revision 1.1  1999/08/18 13:07:03  wenaus
# Computing statistics web page
#
#
######################################################################
#
# stats.pl
#
# T. Wenaus 8/99
#
# Statistics reports
#
# Usage: CGI script
#

use lib "/star/u2d/wenaus/datadb";
require "dbheader.pl";

$curTime = time();

&cgiSetup();

&printMainHeader("STAR Computing Statistics",1);

print "<h3>Home directory usage</h3><ul>\n";
@disks = ('u2','u2a','u2b','u2c','u2d','u2e');
foreach $dk ( @disks ) {
    @dkspace = `cd /star/$dk; df -k /star/$dk`;
    foreach $d ( @dkspace ) {
        if ( $d =~ m/(\d+%)/ ) {$dkfull = $1}
    }
    print "<li><a href=\"/webdata/disk-$dk.txt\">/star/$dk</a> $dkfull\n";
}
print "</ul>\n";

print "<h3>Environment setups</h3>\n<table border=0 cellspacing=12><tr>";
print "<td valign=top><h4>By level</h4><pre>\n";
print `cat /star/starlib/doc/www/html/comp-nfs/swstat-envlevel.txt`.
    "</pre></td>";

print "<td valign=top><h4>By version</h4><pre>\n";
print `cat /star/starlib/doc/www/html/comp-nfs/swstat-envver.txt`.
    "</pre></td>";

print "<td valign=top><h4>By site</h4><pre>\n";
print `cat /star/starlib/doc/www/html/comp-nfs/swstat-envsites.txt`.
    "</pre></td>";

print "<td valign=top><h4><a href=\"/webdata/swstat-envhosts.txt\">By machine</a></h4></td>\n";

print "<td valign=top><h4><a href=\"/webdata/swstat-envusers.txt\">By user</a></h4></td>\n";
print "</tr></table>\n";


print "<h3>root4star invocations (since early Aug '99)</h3>\n<table border=0 cellspacing=12><tr>";
print "<td valign=top><h4>By level</h4><pre>\n";
print `cat /star/starlib/doc/www/html/comp-nfs/swstat-rootlevel.txt`.
    "</pre></td>";

print "<td valign=top><h4>By version</h4><pre>\n";
print `cat /star/starlib/doc/www/html/comp-nfs/swstat-rootver.txt`.
    "</pre></td>";

print "<td valign=top><h4>By site</h4><pre>\n";
print `cat /star/starlib/doc/www/html/comp-nfs/swstat-rootsites.txt`.
    "</pre></td>";

print "<td valign=top><h4><a href=\"/webdata/swstat-roothosts.txt\">By machine</a></h4></td>\n";

print "<td valign=top><h4><a href=\"/webdata/swstat-rootusers.txt\">By user</a></h4></td>\n";
print "</tr></table>\n";

print "<h3>Source code statistics</h3>\<blockquote><pre>";
print `cat /star/starlib/doc/www/html/comp-nfs/swguide-stats.txt`."</pre></blockquote>\n";

print "<h3><a href=\"http://www.star.bnl.gov/cvs/userCommits.html\">Commits by user</a></h3>\n";


#----
print "<p></body></html>\n";
