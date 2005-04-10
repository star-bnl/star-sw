#!/opt/star/bin/perl
#
# $Id: dbheader.pl,v 1.3 1999/07/07 13:22:11 wenaus Exp $
#
# $Log: dbheader.pl,v $
# Revision 1.3  1999/07/07 13:22:11  wenaus
# incorporate run log
#
#
######################################################################
#
# dbheader.pl
#
# T. Wenaus 5/99
#
# Setup material for production database scripts
#
# Usage: required by other scripts
#

use CGI qw(:standard);
use Time::Local;

sub cgiSetup {
    $q=new CGI;
    if ( exists($ENV{'QUERY_STRING'}) ) { print $q->header };
}

sub printMainHeader {
    my ( $title ) = @_;
    if ( @_>1 ) {
        $cache="";
    } else {
        $cache=<<END;
<META HTTP-EQUIV="Expires" CONTENT="Fri, Jun 12 1981 08:20:00 GMT">
<META HTTP-EQUIV="Pragma" CONTENT="no-cache">
<META HTTP-EQUIV="Cache-Control" CONTENT="no-cache">
END
    }
    $curtime = localtime(time());
    print <<END;
<html>
<head>
<title>$title</title>
$cache
<style type="text/css">
//<!--
A:link    {  text-decoration: none}
A:visited {  text-decoration: none}
A:hover   {  text-decoration: none}
A:link.nav {  text-decoration: none}
A:visited.nav {  text-decoration: none}
A:hover.nav {  text-decoration: none}
.nav {  color: #000000}
//-->
</style>
</head>
<body bgcolor=cornsilk text=black link=navy vlink=maroon alink=tomato>
<basefont face="verdana,arial,helvetica,sans-serif">

<!-- Header material -->
<table border=0   cellpadding=5 cellspacing=0 width="100%">
	<tr bgcolor="#ffdc9f">
	<td align=left> <font size="-1">
	<a href="/STAR/">STAR</a>
	&nbsp; <a href="/STAR/html/star_computing.html">Computing</a> 
	</td>
	<td align=right> <font size="-1">
	&nbsp;  <!-- top right corner  --> </font></td>
	</tr>
	<tr bgcolor="#ffdc9f"><td align=center colspan=2><font size="+2"> <b>
	$title
	</b></font></td></tr>
	<tr bgcolor="#ffdc9f">
	<td align=left> <font size="-1">
	<!-- lower left text --> &nbsp; 
	</td>
	<td align=right> <font size="-1"> 
&nbsp; </font></td>
	</tr>
        <tr><td colspan=2 align=right> <font size="-1"> 
Generated at $curtime
 </font></td></tr>
</table>

<p>
<!-- Content -->
END
}

sub printMainFooter {
    print <<END;
</body>
</html>
END
}

1;
