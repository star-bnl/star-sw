#!/opt/star/bin/perl
#
# $Id: dbbrowse.pl,v 1.2 1999/07/07 13:22:10 wenaus Exp $
#
# $Log: dbbrowse.pl,v $
# Revision 1.2  1999/07/07 13:22:10  wenaus
# incorporate run log
#
# Revision 1.1  1999/06/25 15:16:59  wenaus
# Add scripts for managing prod DB and SW guide
#
#
######################################################################
#
# dbbrowse.pl
#
# T. Wenaus 6/99
#
# CGI handler for the production DB query form dbquery.pl
#

require "dbheader.pl";
require "dbsetup.pl";

&cgiSetup();
&printMainHeader("STAR Production Database Query");
$debugOn = 0;

@keyList= ( "beam", "bfc", "geom", "mcgen", "format", "component", "location", "subset", "dataset", "name", "site", "size" );

if ( $debugOn ) {
    $getString = $ENV{'QUERY_STRING'};
    $getString =~ s/%(..)/sprintf("%c", hex($1))/ge;	# unquote %-quoted
    print "\n<b>GET:</b> \"".$getString."\"<br>\n" if $debugOn;
}
if ($qstring = $ENV{'QUERY_STRING'}) {
    foreach (split(/&/, $qstring)) {
        s/%(..)/sprintf("%c", hex($1))/ge;	# unquote %-quoted
        if (/([^=><!]+)=(.*)/) {
            if ($2 ne "") {
                $kyv = $2;
                if (exists($input{$1})) {
                    $input{$1} .= ",$kyv";
                } else {
                    $input{$1} = $kyv;
                }
                print "'$1' = '$input{$1}'<br>\n" if $debugOn;
            }
        } else {
            $input{$_}++;
        }
    }
}

if ( exists($input{"mode"}) ) {
    $mode = $input{"mode"};
} else {
    $mode = "default";
}

if ($mode eq "dataset") {
    $table = $DataSetT;
    $selection = "name,beam,mcgen,param,bimp,geom,had,nfiles,size,comment";
    @badKeys = ("bfc","component","format","location","size");
    $orderBy = "name desc";
} elsif ($mode eq "subset") {
    $table = $SubsetT;
    $selection = "name,cTime,size,nfiles";
    @badKeys = ("bfc","component","format","location","size");
    $orderBy = "cTime desc";
} else {
    $table = $DataFileT;
    $selection = "name,cTime,subset,size";
    @badKeys = 0;
    $orderBy = "cTime desc";
}

if ( exists($input{"limit"}) ) {
    $limit = " limit ".$input{"limit"};
} else {
    $limit = "";
}

for ($i=0; $i<@badKeys; $i++) {
    if ( exists($input{$badKeys[$i]})) {
        print "<b><font color=\"red\">WARNING:</font></b> Key <b>$badKeys[$i]</b> does not exist for table $table. This key will be ignored.<br><br>\n";
        $input{$badKeys[$i]} = "ignore";
    }
}

$selectString = "";

$nSelect = 0;
foreach $qry (sort keys %input) {
    if ($input{$qry} eq "ignore") { next; }
    for ($i=0; $i < @keyList; $i++) {
        if ($qry eq $keyList[$i]) {
            $ky = $keyList[$i];
            @kyval = split(/,/,$input{$ky});
            $selectString .= " and " if ($nSelect > 0);
            if (@kyval>1) {
                $selectString .= " ( ";
                for ($ii=0; $ii<@kyval; $ii++) {
                    if ($ii>0 && $ii<@kyval) { $selectString .= " or "; }
                    $kyv = $kyval[$ii];
                    if ( $kyv =~ m/^([=><]+)/ ) {
                        $oper = "";
                        $kyv =~ s/kB/000/;
                        $kyv =~ s/MB/000000/;
                        $kyv =~ s/GB/000000000/;
                    } else {
                        $oper = "=";
                        $kyv = "'".$kyv."'";
                    }
                    $selectString .= $ky.$oper.$kyv;
                }
                $selectString .= " ) ";
            } else {
                $kyv = $input{$ky};
                if ( $kyv =~ m/^([=><]+)/ ) {
                    $oper = "";
                    $kyv =~ s/kB/000/;
                    $kyv =~ s/MB/000000/;
                    $kyv =~ s/GB/000000000/;
                } else {
                    $oper = "=";
                    $kyv = "'".$kyv."'";
                }
                $selectString .= $ky.$oper.$kyv." ";
            }
            $nSelect++;
        }
    }
}

&StDbConnect();

if ( $selectString ne "" ) {
    if ( exists($input{"selection"}) ) {
        $selectString = "(".$selectString.") and (".$input{"selection"}.")";
    }
    $selectString = "where $selectString";
} else {
    if ( exists($input{"selection"}) ) {
        $selectString = "where ".$input{"selection"};
    }
}

print "<b>Selection from $table table</b><br>&nbsp;&nbsp;&nbsp;<b>$selectString</b><br><b>$limit</b><br>\n";
$sql = "select $selection from " . $table . " $selectString order by $orderBy $limit";
$cursor =$dbh->prepare($sql) 
    || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

$cgipgm = "dummy.pl";
print "\n<pre>\n";
$totsize=0;
$count=0;
while(@fields = $cursor->fetchrow) {
    $anyrecs=1;
    print "\n";
    my $cols=$cursor->{NUM_OF_FIELDS};
    $name = "";
    for($i=0;$i<$cols;$i++) {
        $datatype=$cursor->{TYPE}->[$i]; #ODBC 1==char 11==date 4==int -1==text
        if($datatype == -1 || $datatype==252) {next}
        my $fvalue=$fields[$i];
        my $fname=$cursor->{NAME}->[$i];
        #print $fname,"datatype=",$datatype;
        print " ";
        $fn=lc($fname);
        $fn=lc($fname);
        print "$fn = $fvalue<br>" if $debugOn;
        if ($fn eq "name") {            
            if ($mode eq "dataset") {
                printf("<b>%10s</b>",$fvalue);
                $ds = $fvalue;
                print " <a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/browse.pl?mode=subset&dataset=$fvalue\">subsets</a>";
                print " <a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/browse.pl?dataset=$fvalue\">files</a>";
            } elsif ($mode eq "subset") {
                printf("<b>%20s</b>",$fvalue);
                print " <a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/browse.pl?subset=$fvalue\">files</a>";
            } else {
                $name = $fvalue;
            }
        } elsif ($fn eq "size") {
            $totsize += $fvalue;
            printf(" %8d kB",$fvalue/1000);
        } elsif ($fn eq "ctime") {
            print substr($fvalue,2,14);
        } elsif ($fn eq "nfiles") {
            printf("%3d files",$fvalue);
        } elsif ($fn eq "subset") {
            print " <a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/browse.pl?subset=$fvalue\">subset</a>";
        } elsif($fn eq 'userkey' || $fn eq 'password'|| $fn eq 'date_created' || $fn eq 'date_updated') {
            next;
        } elsif($fn eq 'id') { #generate a submit href
            print "<a href=\"",$cgipgm,"&id=",$fvalue,"\">&nbsp; ",$fields[$i],"&nbsp;</a>";
        } elsif ($fn eq 'link') { #generate a link href
            print '<a href="',$fvalue,'">';
            if(length($fvalue)>0) {print 'click'}
            print '</a>';
        } else {
            $fvalue =~ s/ronic_/_/;  # clean up 'hadronic_on'
            printf("%10s",$fvalue);
        }
    }
    if ($name ne "") { print " ".$name };
#    if ($mode eq "dataset") { &getSubsets($ds); }
    $count++;
}
print "</pre><p>\n";
printf("<b>Count: %8d</b> &nbsp; &nbsp;\n",$count);
if ($totsize>0) {
    $totsize = $totsize/1000000;
    printf("<b>Total size %8d MB</b>\n",$totsize);
}
if(!$anyrecs && !(defined $embed)) {print "<p>No records matched your query - try removing some of your search criteria";}

$cursor->finish;
&StDbDisconnect;

&printMainFooter();
exit;

####################
#Generates an html table from a cursor
#Parameter is the name of the cgi to execute for the detail view (one record per page)
sub StGenTable {
	my ($cgipgm,$embed)=@_;
	my $cols=$cursor->{NUM_OF_FIELDS};
	$n='';
	print "<table border=1",$n,">";
	my $i=0;
	print "\n<tr>";
	for($i=0;$i<$cols;$i++) {
		my $fname=$cursor->{NAME}->[$i];
		$datatype=$cursor->{TYPE}->[$i]; #ODBC 1==char 11==date 4==int -1==text
		if($datatype == -1 || $datatype==252) {next} #don't show longvarchars
		$fn=lc($fname);
		if($fn eq 'userkey' || $fn eq 'password' || $fn eq 'date_created' || $fn eq 'date_updated') {next};
		$fname=~s/_/ /g;
		print "<td bgcolor=\"#FFFFCC\"><b>",$fname,"</b></td>";
	}
	print("</tr>");
	
	my $anyrecs=0;
	while(@fields = $cursor->fetchrow) {
		$anyrecs=1;
		print "\n<TR>";
		my $cols=$cursor->{NUM_OF_FIELDS};
		for($i=0;$i<$cols;$i++) {
			$datatype=$cursor->{TYPE}->[$i]; #ODBC 1==char 11==date 4==int -1==text
			if($datatype == -1 || $datatype==252) {next}
			my $fvalue=$fields[$i];
			my $fname=$cursor->{NAME}->[$i];
			#print $fname,"datatype=",$datatype;
			print "<td nowrap>";
			$fn=lc($fname);
			$fn=lc($fname);
			if($fn eq 'userkey' || $fn eq 'password'|| $fn eq 'date_created' || $fn eq 'date_updated') {next};
			if($fn eq 'id') { #generate a submit href
				print "<a href=\"",$cgipgm,"&id=",$fvalue,"\">&nbsp; ",$fields[$i],"&nbsp;</a>";
			} elsif ($fn eq 'link') { #generate a link href
				print '<a href="',$fvalue,'">';
				if(length($fvalue)>0) {print 'click'}
				print '</a>';
			} else {
				print $fvalue;
				#print $datatype;
			}
			print "</td>";
		}
		print("</TR>");
	}
	print "</table><p>\n";
	if(!$anyrecs && !(defined $embed)) {print "<p>No records matched your query - try removing some of your search criteria";}
	
}


print <<END;

<h2> table=$input{"mcgen"} </h2>
</body>
</html>
END
    exit;

#####################
sub getSubsets {
    my ( $setname ) = @_;
    my $sql, $cursor2, @fields2, $i;
    print "\n      ";
    $sql = "select name from ".$SubsetT." where dataset='$setname'";
    $cursor2 =$dbh->prepare($sql) 
        || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor2->execute;
    while(@fields2 = $cursor2->fetchrow) {
        my $cols=$cursor->{NUM_OF_FIELDS};
        for($i=0;$i<$cols;$i++) {
			my $fvalue=$fields2[$i];
			my $fname=$cursor2->{NAME}->[$i];
			$fn=lc($fname);
			if($fn eq 'name') {
                print "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/browse.pl?subset=$fvalue\">$fvalue</a>  ";
            }
        }
    }
}
