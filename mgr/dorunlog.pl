#!/opt/star/bin/perl
#
# $Id: dorunlog.pl,v 1.2 1999/09/21 12:26:40 wenaus Exp $
#
# $Log: dorunlog.pl,v $
# Revision 1.2  1999/09/21 12:26:40  wenaus
# Add calib/param databases to backup list
#
# Revision 1.1  1999/07/07 13:19:30  wenaus
# real data log
#
#
######################################################################
#
# dorunlog.pl
#
# T. Wenaus 6/99
#
# Store a run log entry in the database
#
# Usage: 'POST' form processor (form set up in dbrunlog.pl)
#

require "dbheader.pl";
require "dbsetup.pl";

$debugOn = 1;

&cgiSetup();
&printMainHeader("Online log submission",1);

@myparams = ("user","name","type","trig","zerosup","pedsub","rawformat","gainmode","field","thrlo","thrhi","seqlo","seqhi","title","comment");

for ($ii=0; $ii<@myparams; $ii++) {
    print "Param ".$myparams[$ii]." = ".param($myparams[$ii])."<br>\n" if $debugOn;
}

## connect to the DB
&StDbConnect();
print "DB connected<br>\n" if $debugOn;

## First check whether an entry exists for this run number
if ( param('name' ne '' ) ) {
    $sql="select name,endtime from $RunT where name='".param('name')."'";
    $cursor =$dbh->prepare($sql) 
        || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;
    $nrow = 0;
    while(@fields = $cursor->fetchrow) {
        $nrow++;
        my $cols=$cursor->{NUM_OF_FIELDS};
        for($i=0;$i<$cols;$i++) {
            my $fvalue=$fields[$i];
            my $fname=$cursor->{NAME}->[$i];
            $fn=lc($fname);
            $val{$fn} = $fvalue;
        }
    }
}

($sec,$min,$hr,$dy,$mo,$yr,$wkd,$ydy,$isdst) =
    localtime();
$ctime = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                      $yr+1900,$mo+1,$dy,$hr,$min,$sec);

# add the log entry
$sql="insert into $RunT  set ";
$sql.="user='".param("user")."',";
$sql.="name='".param("name")."',";
$sql.="ctime=$ctime,";
$sql.="starttime=$ctime,";
$sql.="type='".param("type")."',";
$sql.="trig='".param("trig")."',";
$sql.="events='".param("events")."',";
$sql.="stage='daq',";
$sql.="zerosup='".param("zerosup")."',";
$sql.="pedmode='".param("pedmode")."',";
$sql.="rawformat='".param("rawformat")."',";
$sql.="gainmode='".param("gainmode")."',";
if ( param("field") ne '' ) {$sql.="field=".param("field").","}
$sql.="thrlo=".param("thrlo").",";
$sql.="thrhi=".param("thrhi").",";
$sql.="seqlo=".param("seqlo").",";
$sql.="seqhi=".param("seqhi").",";
$sql.="title='".param("title")."',";
$sql.="comment=".$dbh->quote(param("comment"));
print "$sql<br>\n" if $debugOn;
open(ARCHIVE,">>/star/sol4/duvall/archive/runlog.archive");
print ARCHIVE $sql."\n";
close(ARCHIVE);
print "===".$dbh->quote(param("comment"))."===<br>\n";
$rv = $dbh->do($sql) || print "<br><b>".$dbh->errstr."</b><br>";

# finished
&StDbDisconnect();
print "DB disconnected<br>\n" if $debugOn;

print <<END;
<p>
Submitted to database.
</body>
</html>
END
exit;
