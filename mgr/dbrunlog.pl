#!/opt/star/bin/perl
#
# $Id: dbrunlog.pl,v 1.3 1999/07/10 13:18:06 wenaus Exp $
#
# $Log: dbrunlog.pl,v $
# Revision 1.3  1999/07/10 13:18:06  wenaus
# add 'run only' and 'log only' views
#
# Revision 1.2  1999/07/09 12:42:31  wenaus
# Change default seq limits
#
# Revision 1.1  1999/07/07 13:19:30  wenaus
# real data log
#
#
######################################################################
#
# dbrunlog.pl
#
# T. Wenaus 6/99
#
# Present run log submission form and content display
#
# Usage: CGI script
#

use lib "/star/u2d/wenaus/datadb";
require "dbheader.pl";
require "dbsetup.pl";

$debugOn = 0;

&cgiSetup();

@paramlist = $q->param();
foreach $par ( @paramlist ) {
    $logOnly = 1 if ( $par eq 'log' );
    $runOnly = 1 if ( $par eq 'run' );
    $showLog = 1 if ( $par eq 'show' );
}

$logLogUrl = "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/dbrunlog.pl?show=yes&log\">Comments only</a>";
$runLogUrl = "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/dbrunlog.pl?show=yes&run\">Run log only</a>";
$fullLogUrl = "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/dbrunlog.pl?show=yes\">Full log</a>";
if ( $showLog ) {
    if ( $logOnly ) {
        $title = "Online comment log";
        $logLogUrl = "Comments only";
    } elsif ( $runOnly ) {
        $title = "Run log";
        $runLogUrl = "Run log only";
    } else {
        $title = "Run log and online comments";
        $fullLogUrl = "Full log";
    }
    &printMainHeader($title,1);
    &displayLog();
} elsif ( $q->param('events') ne '') {
    &printMainHeader("Events for run ".$q->param('events'));
    &displayEvents($q->param('events'));
} else {
    &printMainHeader("Online Run Log and Comment Log");
    &logEntryForm();
}

print "</body></html>\n";
exit;

######################################################################
sub displayLog {
    @daqfiles = </disk1/star/daq/*.daq>;
    # connect to the DB
    &StDbConnect();
    $table = $RunT;
    $selection = "*" ;
    $selectString = "";
#    $selectString = "where ";
    $orderBy = "starttime desc";
    if ( $q->param('limit') ne '' ) {
        $limit = " limit ".$q->param('limit');
    } else {
        $limit = "";
    }
    $sql = "select $selection from $table $selectString order by $orderBy $limit";
    $cursor =$dbh->prepare($sql) 
        || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;

    print <<END;
<center><font size="-1">
<a href="http://duvall.star.bnl.gov/cgi-bin/prod/dbrunlog.pl">
Run log entry form</a> -
<a href="http://duvall.star.bnl.gov/HyperNews/get/commsched.html">
Commissioning forum</a> -
<a href="http://redford.star.bnl.gov/staronline">Online</a> -
<a href="http://daq.star.bnl.gov/~daq/">DAQ</a> -
<a href="/STARAFS/comp/prod/">Production</a> -
<a href="http://redford.star.bnl.gov/staronline/shifts/shiftLogReport.htm">
Old log</a>
</font></center>
<p>
Links at right (you need a wide window) give access to event summaries and allow editing/deleting
of run log entries. To kill a junk entry like a 'testing' comment,
edit it and set status=-1 (if you just delete it it will reappear if
the database is rebuilt from the entry log). Updated hourly.
<p>
<font size="-1">$fullLogUrl - $runLogUrl - $logLogUrl</font>
<hr>
<p><pre>
END
    $nrow=0;
    while(@fields = $cursor->fetchrow) {
        my $cols=$cursor->{NUM_OF_FIELDS};
        for($i=0;$i<$cols;$i++) {
            $datatype=$cursor->{TYPE}->[$i]; #ODBC 1==char 11==date 4==int -1==text
#            if($datatype == -1 || $datatype==252) {next}

            my $fvalue=$fields[$i];
            my $fname=$cursor->{NAME}->[$i];
            $fn=lc($fname);
            $val{$fn} = $fvalue;
            print "$fn=$fvalue " if $debugOn;
            if($debugOn && ($datatype == -1 || $datatype==252)) {
                print "\n$fn $datatype $fvalue\n";
            }
        }
        $nrun = $val{'name'};
        if ( $nrun ne '' && $nrun < 100 ) {next}  # Don't display junk
        $id=$val{'id'};
        $delLink="<a href=\"http://duvall.star.bnl.gov/phpMyAdmin/sql.php3?&server=0&db=system_data&table=Run&goto=%2FphpMyAdmin%2Fsql.php3%3Fserver%3D0%26db%3Dsystem_data%26pos%3D0%26sql_query%3DSELECT%2B%252A%2BFROM%2BRun%26sql_order%3D%2Border%2Bby%2B%2527ctime%2527%2BASC%26table%3DRun&sql_query=DELETE+FROM+Run+WHERE++id+%3D+%27$id%27+&zero_rows=The+row+has+been+deleted\"><u>Del</u></a>";
        $editLink="<a href=\"http://duvall.star.bnl.gov/phpMyAdmin/tbl_change.php3?&server=0&db=system_data&table=Run&goto=%2FphpMyAdmin%2Fsql.php3%3Fserver%3D0%26db%3Dsystem_data%26pos%3D0%26sql_query%3DSELECT%2B%252A%2BFROM%2BRun%26sql_order%3D%2Border%2Bby%2B%2527ctime%2527%2BASC%26table%3DRun&primary_key=+id+%3D+%27$id%27+\"><u>Edit</u></a>";
        $eventsLink="<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/dbrunlog.pl?events=$nrun\"><u>Events</u></a>";
        $outline = '';
        if ($val{'name'} eq '') {
            if (! $runOnly) {
                ## if a comment, don't print if status<0
                if ($val{'status'} >= 0) {
                    ## If not a run record, don't print the run params
                    $outline = sprintf(&lineFormat("darkgreen","darkgreen",1),
                                       'Comment',
                                       substr($val{'user'},0,15),
                                       substr($val{'starttime'},2,14),
                                       '',
                                       '',
                                       '',
                                       '',
                                       '',
                                       '',
                                       '',
                                       '',
                                       '',
                                       '','',
                                       '',
                                       $editLink,
                                       $delLink
                                       );
                }
            }
        } else {
            if ( ! $logOnly ) {
                if ( $val{'field'} eq '' ) {
                    $pct='';
                } else {
                    $pct='%';
                }
                $outline = sprintf(&lineFormat("darkgreen","darkgreen",1),
                                   $val{'name'},
                                   substr($val{'user'},0,15),
                                   substr($val{'starttime'},2,14),
                                   $val{'nevents'},
                                   $val{'trig'},
                                   $val{'zerosup'},
                                   $val{'pedmode'},
                                   $val{'gainmode'},
                                   $val{'thrlo'},
                                   $val{'thrhi'},
                                   $val{'seqlo'},
                                   $val{'seqhi'},
                                   $val{'field'},$pct,
                                   $val{'rawformat'},
                                   $eventsLink,
                                   $editLink,
                                   $delLink
                                   );
            }
        }
        if ( $outline ne '' ) {
            $nrow++;
            if ($nrow%8 == 1) {
                if ( $logOnly ) {
                    printf(&lineFormat("blueviolet","blueviolet",1),
                           '',
                           'Submitter',
                           'Submitted at',
                           '',
                           '',
                           '',
                           '',
                           '',
                           '',
                           '',
                           '',
                           '',
                           '','',
                           ''
                           );
                } else {
                    printf(&lineFormat("blueviolet","blueviolet",1),
                           'Run',
                           'Submitter',
                           'Submitted at',
                           'Nevents',
                           'Trig',
                           'Zero',
                           'Ped',
                           'Gain',
                           'Lo',
                           'Hi',
                           'Lo',
                           'Hi',
                           'Field','',
                           'Raw'
                           );
                    printf("<font color=\"blueviolet\"><b>%66s %26s %4s %11s</b></font>\n",
                           'Sup','Thres','Seq','Fmt');
                }
            }
            print $outline;
            if ($val{'title'} ne '') {
                print "<blockquote><b>".$val{'title'}."</b></blockquote>";
            }
            if ($val{'comment'} ne '') {
                print "<blockquote><blockquote>";
                print $val{'comment'};
                print "</blockquote></blockquote>";
            }
            if ( @daqfiles>0 ) {
                print "<blockquote><blockquote>";
                foreach $daqf ( @daqfiles ) {
                    my $name = $val{'name'};
                    if ( $daqf =~ m/\.$name\.daq$/ ) {
                        my ($fmode, $uid, $gid, $filesize, 
                            $readTime, $writeTime, $cTime) =
                                (stat($daqf))[2,4,5,7,8,9,10];
                        printf("<b>Data: %-28s %6dMB</b>\n",$daqf,$filesize/1000000);
                    }
                }
                print "</blockquote></blockquote>";
            }
        }
    }
    print "</pre><p>\n";

    # finished
    &StDbDisconnect();
}

######################################################################

sub logEntryForm {
print <<END;
<center><font size="-1">
<a href="http://duvall.star.bnl.gov/HyperNews/get/commsched.html">
Commissioning forum</a> -
<a href="http://redford.star.bnl.gov/staronline">Online</a> -
<a href="http://daq.star.bnl.gov/~daq/">DAQ</a> -
<a href="/STARAFS/comp/prod/">Production</a>
</font></center>
<h3>Browse the log:</h3>
<blockquote>
<h3>
$fullLogUrl - $runLogUrl - $logLogUrl - 
<a href="http://redford.star.bnl.gov/staronline/shifts/shiftLogEntry.htm">Old log</a></h3>
</h3>
</blockquote>
<h3>Make a log entry:
<font color="red">Continue to use the
<a href="http://redford.star.bnl.gov/staronline/shifts/shiftLogEntry.htm">old log</a> for the moment to make entries)</font>
</h3>
<blockquote>
<font color="red"><b>Red: Required for logging comments or runs</b></font>
<br>
<font color="darkgreen"><b>Green: Use these if you are logging a run</b></font>


<form action="http://duvall.star.bnl.gov/cgi-bin/prod/dorunlog.pl" method="post">
<table border=1 cellpadding=2 cellspacing=2>
<tr>
<td><b><font color="red">Your name:</font></b></td><td>
<input type="text" size=30 name="user"></td>
</tr><tr>
<td><b><font color="darkgreen">Run number:</font></b></td><td>
<input type="text" size=15 name="name"></td>
</tr><tr>
<td><b><font color="darkgreen">Run type:</font></b></td><td>
<select name="type">
END

foreach $rtyp (sort keys %runTypes) {
    print "<option value=\"".$rtyp."\">".$runTypes{$rtyp}."\n";
}
print <<END;
</select>
</td>
</tr><tr>
<td><b><font color="darkgreen">Trigger type:</font></b></td><td>
<select name="trig">
END

foreach $ttyp (sort keys %trigTypes) {
    print "<option value=\"".$ttyp."\">".$trigTypes{$ttyp}."\n";
}
print <<END;
</select>
</td>
</tr><tr>
<td><b><font color="darkgreen">Number of events:</font></b></td><td>
<input type="text" name="nevents" size=7>
</td></tr><tr>
<td><b><font color="darkgreen">DAQ configuration:</font></b></td><td>
<input type="checkbox" name="zerosup" value="Y"> Zero suppressed
<br>
<input type="checkbox" name="pedmode" value="on"> Pedestal subtracted
<br>
<input type="checkbox" name="rawformat" value="Y"> Raw format
<br>
Gain mode <select name="gainmode">
END

foreach $gm (sort keys %gainModes) {
    print "<option value=\"".$gm."\">".$gainModes{$gm}."\n";
}
print <<END;
</select>
</tr><tr>
<td><b><font color="darkgreen">ASIC parameters:</font></b></td><td>
<table border=0 width="100%"><tr><td align=right>
Threshold high <input type="text" name="thrhi" value=12 size=4>
</td><td align=right>
Sequence limit high <input type="text" name="seqhi" value=3 size=4>
</td></tr><tr><td align=right>
Threshold low <input type="text" name="thrlo" value=4 size=4>
</td><td align=right>
Sequence limit low <input type="text" name="seqlo" value=0 size=4>
</td></tr></table>
</td>
</tr><tr>
<td><b><font color="darkgreen">Detector configuration:</font></b></td><td>
Magnetic field <input type="text" name="field" size=4>% of full field
</td>
</tr><tr><td>
<b><font color="red">Title or activity:</font></b>
</td><td>
<input type="text" name="title" size="48">
</td></tr><tr><td colspan=2>
<b><font color="red">Comments:</font></b><br>
<textarea rows=10 cols=72 name="comment"></textarea>
</tr><tr><td colspan=2>
<center>    <input type="submit" value="Submit"> &nbsp; <input type="reset"> </center>
</td></tr></table>
</blockquote>
END
}

###########################################
sub lineFormat {
    my ( $col1, $col2, $bold ) = @_;
    my $c1 = "<font color=\"$col1\">";
    my $c2 = "<font color=\"$col2\">";
    my $ce = "</font>";
    if ( $bold ) {
        $b1="<b>";
        $be="</b>";
    } else {
        $b1='';
        $be='';
    }
    my $fmt = "$b1$c1%-12s$ce$c2 %-15s$ce$c1 %-14s$ce$c2 %-7s$ce$c2 %-10s$ce$c1 %-4s$ce$c2 %-7s$ce$c1 %-11s$ce$c2 %2s$ce$c1 %2s$ce$c2 %2s$ce$c1 %2s$ce$c2 %5s%1s$ce$c1 %-3s$ce$c2 %s$ce$c1 %s$ce$c2 %s$ce$be\n";
    return $fmt;
}

###########################################
sub displayEvents {
    my ( $runno ) = @_;
    # connect to the DB
    &StDbConnect();
    $table = $EventT;
    $selection = "*" ;
    $selectString = "where nrun='$runno'";
    $orderBy = "time";
    if ( $q->param('limit') ne '' ) {
        $limit = " limit ".$q->param('limit');
    } else {
        $limit = "";
    }
    $sql = "select $selection from $table $selectString order by $orderBy $limit";
    $cursor =$dbh->prepare($sql) 
        || print "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;

    print "<p><pre>\n";
    $nrow=0;
    while(@fields = $cursor->fetchrow) {
        my $cols=$cursor->{NUM_OF_FIELDS};
        for($i=0;$i<$cols;$i++) {
            $datatype=$cursor->{TYPE}->[$i]; #ODBC 1==char 11==date 4==int -1==text
#            if($datatype == -1 || $datatype==252) {next}

            my $fvalue=$fields[$i];
            my $fname=$cursor->{NAME}->[$i];
            $fn=lc($fname);
            $val{$fn} = $fvalue;
            if ($fn eq 'trigwd' || $fn eq 'trigwdin' ) {
                $val{$fn} = sprintf("%x",$val{$fn});
            }
            if ($val{$fn} ne '' && $val{$fn} ne 'unknown' )
              {print "$fn=".$val{$fn}." "}
            if($debugOn && ($datatype == -1 || $datatype==252)) {
                print "\n$fn $datatype $fvalue\n";
            }
        }
        print "\n";
    }
    print "</pre><p>\n";

    # finished
    &StDbDisconnect();
}
