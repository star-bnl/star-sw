#!/opt/star/bin/perl
#
# $Id: dbquery.pl,v 1.4 1999/09/21 12:26:39 wenaus Exp $
#
# $Log: dbquery.pl,v $
# Revision 1.4  1999/09/21 12:26:39  wenaus
# Add calib/param databases to backup list
#
# Revision 1.3  1999/08/08 18:54:38  wenaus
# Add dubna
#
# Revision 1.2  1999/07/07 13:22:11  wenaus
# incorporate run log
#
#
######################################################################
#
# dbquery.pl
#
# T. Wenaus 5/99
#
# Production database query form
#
# Usage: CGI script
#

require "dbheader.pl";
require "dbsetup.pl";

&cgiSetup();
&printMainHeader("STAR Production Database Query Form");

%keys = (
         "beam" => "auau50,auau100,auau200,augas100,pa200,pau200,pp200,sisi200,cosmics",
         "geom" => "year1a,year1b,year1s,year2a,year2b",
         "mcgen" => "hbt_ven,hbt_vni,hijet,hijing,hijing135,muon,pythia,two_photon,venus412,vni",
         "bfc" => "evg,gst,tfs,trs,tss",
         "format" => "daq,xdf,fz,fzd,root,root-MDC2",
         "component" => "dst,event,geant,hist,tpc_hits,tpc_tracks,trg,l3t",
         "site" => "psc,set,ric,rcf,uta,pet,dub",
         "location" => "hpss-rcf,disk-rcf",
         "size" => "=0,<1kB,>100MB,>1GB"
         );
%keycolor = (
             "beam" => "green",
             "geom" => "green",
             "mcgen" => "green",
             "bfc" => "blue",
             "format" => "blue",
             "component" => "blue",
             "site" => "green",
             "location" => "blue",
             "size" => "blue"
             );

print <<END;
<center><font size="-1">
<a href="http://duvall.star.bnl.gov/cgi-bin/prod/dbrunlog.pl?show=yes">
Real data run log</a> - 
<a href="/STARAFS/comp/prod/">Production</a>
</font></center>
<p>
 This is an interface to the <a href="http://www.mysql.org/">MySQL</a>
based production database and data catalog.
The database is updated nightly at present.
All comments (to wenaus\@bnl.gov) appreciated.
 <ul>
     <li> Files with paths starting with <code>/home</code> are in HPSS.
     If you need HPSS-resident files for which a copy doesn't exist on
     disk you need to ask Lidia (Didenko\@mail.rhic.bnl.gov) to stage
     them in for you
     <li> Some keys are applicable only for files.
     <font color="green">Green: files and datasets</font>;
     <font color="blue">Blue: files only</font>
     <li> Other sources for test and sample data files are
     <ul>
       <li><a href="/data/disk1_star_test/">/disk1/star/test</a>
       <li><a href="/data/afs_data_samples/">/afs/rhic/star/data/samples</a>
       <li><a href="/data/afs_data_test/">/afs/rhic/star/data/test</a>
     </ul>
 </ul>

<p>
END

print <<END;


<table border="1" cellpadding="5" cellspacing="0" width="100%">
  <tr> <td valign="middle" bgcolor="whitesmoke"><font size="+2">
  Select by key(s): (you can select multiple keys)
</font></td></tr></table>
<form action="http://duvall.star.bnl.gov/cgi-bin/prod/dbbrowse.pl" method="get">
<table border="1" width="100%">
END

foreach $ky (sort keys %keys) {

    print <<END;
<tr><td>
<b><font color="$keycolor{$ky}">$ky</font></b>
</td><td>
<table border=0 width="100%" cellpadding=0 cellspacing=0>
<tr>
END

    $sublist = $keys{$ky};
    @subdoms = split(',',$sublist);
    for ($i=0; $i<@subdoms; $i++) {
        $subd = $subdoms[$i];
        print <<END;
<td width="25%">
<font size="-1">
        <input type="checkbox" name="$ky" value="$subd"> $subd
</font>
</td>
END
      if ( ($i+1)%4 == 0) {
         print "</tr><tr>\n";
      }
    }
    for ($k=0; $k<(4-@subdoms)%4; $k++) { print "<td width=\"25%\">&nbsp;</td>\n"; }
    print "</tr></table></td></tr>\n";
}

print <<END;
</table>
<b>You can enter your own SQL here</b>
<input type="text" name="selection" size=50>
<br>
<b>Limit record count returned to</b>
<input type="text" name="limit" size=7 value="100000">
<br>
<b>Search for
<INPUT TYPE="radio" NAME="mode" VALUE="default" CHECKED> files
&nbsp;<INPUT TYPE="radio" NAME="mode" VALUE="dataset"> datasets</b>
<br><input type="submit"> &nbsp; <input type="reset">
</form>

<table border="1" cellpadding="5" cellspacing="0" width="100%">
  <tr> <td valign="middle" bgcolor="whitesmoke"><font size="+2">
Find dataset: (eg. psc0194)
</font></td></tr></table>
<form action="http://duvall.star.bnl.gov/cgi-bin/prod/dbbrowse.pl" method="get">
<b>Dataset:</b> <input type="text" name="name">
<input type="hidden" name="mode" value="dataset">
<input type="submit">
</form>

<table border="1" cellpadding="5" cellspacing="0" width="100%">
  <tr> <td valign="middle" bgcolor="whitesmoke"><font size="+2">
Find subset files: (eg.  psc0194_03_40evts)
</font></td></tr></table>
<form action="http://duvall.star.bnl.gov/cgi-bin/prod/dbbrowse.pl" method="get">
<b>Subset:</b> <input type="text" name="subset">
<input type="submit">
</form>

<table border="1" cellpadding="5" cellspacing="0" width="100%">
  <tr> <td valign="middle" bgcolor="whitesmoke"><font size="+2">
<a href="http://duvall.star.bnl.gov/phpMyAdmin/index.php3">
DB administration browser</a>
</font></td></tr></table>
END

print <<END;
</body>
</html>
END
