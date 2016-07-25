#!/usr/local/bin/perl
#!/usr/bin/env perl 
#
# $Id: dbDevTestQueryPlot.pl,v 1.1 2013/11/12 19:55:34 fisyak Exp $
#
# $Log: dbDevTestQueryPlot.pl,v $
# Revision 1.1  2013/11/12 19:55:34  fisyak
# Freeze
#
# Revision 1.79  2013/06/28 18:21:19  didenko
# remove Mysql
#
# Revision 1.78  2013/06/25 18:10:22  didenko
# remove use:Mysql
#
# Revision 1.77  2012/05/21 20:09:26  didenko
# updated title
#
# Revision 1.75  2012/05/21 19:20:56  didenko
# updated for ration RealTime/CPU
#
# Revision 1.74  2012/05/18 20:33:38  didenko
# try to set min, max
#
# Revision 1.67  2012/05/18 18:13:44  didenko
# removed plots for tpt tracking
#
# Revision 1.66  2012/05/14 16:13:59  didenko
# comment unusable fields
#
# Revision 1.65  2012/05/09 15:44:39  didenko
# minor fixes
#
# Revision 1.64  2012/05/09 15:17:26  didenko
# move db connection to this script
#
# Revision 1.63  2012/05/07 19:25:08  didenko
# add color
#
# Revision 1.62  2012/05/07 15:06:41  didenko
# added AgML test
#
# Revision 1.61  2012/05/07 14:44:27  didenko
# added AgML test
#
# Revision 1.59  2012/03/29 20:10:40  didenko
# add AgML test
#
# Revision 1.57  2010/10/29 15:39:31  didenko
# change mark size
#
# Revision 1.56  2010/10/29 15:37:00  didenko
# change color order
#
# Revision 1.55  2010/10/29 15:33:30  didenko
# remove tpt plots
#
# Revision 1.54  2010/07/02 17:11:29  didenko
# updated for number of vertices
#
# Revision 1.53  2009/12/03 22:11:01  didenko
# more fixes
#
# Revision 1.50  2008/03/31 20:07:30  didenko
# more changes for updated values
#
# Revision 1.49  2008/03/31 19:21:31  didenko
# extended set of averaged values for usable events
#
# Revision 1.48  2008/01/09 20:40:49  didenko
# updated due to moved directory
#
# Revision 1.47  2007/11/07 16:43:02  didenko
# last cleanup for working version
#
# Revision 1.39  2006/07/25 19:36:15  didenko
# more updates
#
# Revision 1.38  2006/07/21 19:22:02  didenko
# come back to previous version
#
# Revision 1.34  2006/07/21 18:55:44  didenko
# more fixes
#
# Revision 1.30  2006/04/14 16:20:23  didenko
# updated for tracks with nfit point > 15
#
# Revision 1.29  2005/01/10 18:02:43  didenko
# remove icc path
#
# Revision 1.23  2005/01/10 15:28:47  didenko
# updated for ITTF test
#
# Revision 1.22  2004/12/20 22:28:58  didenko
# more directories to query
#
# Revision 1.21  2004/12/20 21:35:55  didenko
# comment print
#
# Revision 1.20  2004/12/20 21:32:02  didenko
# updated for new datasets and SL3 platform
#
# Revision 1.19  2004/02/16 04:13:49  jeromel
# Small modifs (modules would need to be also installed in OPTSTAR)
#
# Revision 1.18  2002/10/11 15:13:42  didenko
# *** empty log message ***
#
# Revision 1.14  2002/01/30 17:39:55  didenko
# extand week days for Sat, Sun
#
# Revision 1.12  2001/06/07 17:08:18  jeromel
# Change DEV00 -> dev
#
# Revision 1.11  2001/02/27 16:38:05  liuzx
# max_y and min_y changed! (9th and 6th ticks)
#
# Revision 1.10  2001/02/23 00:46:06  liuzx
# Now output the GIF on the fly!
#
#
##########################################################


BEGIN {
 use CGI::Carp qw(fatalsToBrowser carpout);
}

use DBI;
use CGI qw(:standard);
use GD;
use GD::Graph::linespoints;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="LibraryJobs";

$JobStatusT = "JobStatus";

my $query = new CGI;

my $day_diff = 8;
my $max_y = 0,
my $min_y = 500000;
my @data;
my @legend;

my %plotHash = (
                MemUsage => 'memUsageF, memUsageL',
                CPU_per_Event => 'CPU_per_evt_sec',
		RealTime_per_Event => 'RealTime_per_evt',
                RealTime_per_CPU => 'RealTime_per_evt, CPU_per_evt_sec',  
                Average_NoTracks => 'avg_no_tracks',
		Average_NoPrimaryT => 'avg_no_primaryT',
                Average_NoTracksNfit15 => 'avg_no_tracksnfit15',
		Average_NoPrimaryTNfit15  => 'avg_no_primaryTnfit15',     
                Average_NoPrimVertex => 'avgNoVtx_evt',
                NoEvent_vertex => 'NoEventVtx',
                Average_NoV0Vrt => 'avg_no_V0Vrt',
		Average_NoXiVrt => 'avg_no_XiVrt',
                Percent_of_usableEvents => 'percent_of_usable_evt',
                Average_NoTracks_per_usableEvent => 'avgNoTrack_usbevt',
#                Average_NoPrimTrack_per_usableEvent => 'avgNoPrTrack_1vtx_usbevt',
                Average_NoTracksNfit15_per_usableEvent => 'avgNoTrackNfit15_usbevt',
#                Average_NoPrimTrackNfit15_per_usableEvent => 'avgNoPrTrackNfit15_1vtx_usbevt',
#                Average_NoV0_per_usableEvent => 'avgNoV0_usbevt',
#                Average_NoXi_per_sableEvent => 'avgNoXi_usbevt',
               );

my $set1    =  $query->param('set1');
my $plotVal = $query->param('plotVal');
my $weeks   = $query->param('weeks');

if ( ($set1 eq "") || ($plotVal eq "") ) {
    print $query->header;
    print $query->start_html('Plot for Nightly Test in DEV Library');
    print "<body bgcolor=\"cornsilk\"><center><pre>";
    print "<h1>You must select both the type of test and plot!!</h1>";
    print $query->end_html;
    exit(0);
}

my @point0 = ();
my @point1 = ();
my @point2 = ();
my @point3 = ();
my @point4 = ();
my @point5 = ();
my @point6 = ();
my @point7 = ();
my @point8 = ();
my @point9 = ();
my @point10 = ();
my @point11 = ();
my @point12 = ();
my @point13 = ();
my @point14 = ();
my @point15 = ();
my @point16 = ();
my @point17 = ();


my @Nday;
for($i=0;$i<7*$weeks;$i++) {
    $point0[$i]=undef;
    $point1[$i]=undef;
    $point2[$i]=undef;
    $point3[$i]=undef;
    $point4[$i]=undef;
    $point5[$i]=undef;
    $point6[$i]=undef;
    $point7[$i]=undef;
    $point8[$i]=undef;
    $point9[$i]=undef;
    $point10[$i]=undef;
    $point11[$i]=undef;
    $point12[$i]=undef;
    $point13[$i]=undef;
    $point14[$i]=undef;
    $point15[$i]=undef;
    $point16[$i]=undef;
    $point17[$i]=undef;
    $Nday[$i] = undef;
}

  my @spl = ();
 @spl = split(" ",$plotVal);
 my $plotVl = $spl[0];

 my $mplotVal = $plotHash{$plotVl};

($today,$today,$today,$mday,$mon,$year,$today,$today,$today) = localtime(time);
#$sec,$min,$hour                  $wday,$yday,$isdst
$today = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];
my $nowdate = ($year+1900)."-".($mon+1)."-".$mday;

if ( $today eq Tue ) {
    $Nday[0] = "Tue"; $Nday[1] = "Wed"; $Nday[2] = "Thu"; $Nday[3] = "Fri"; $Nday[4] = "Sat"; $Nday[5] = "Sun"; $Nday[6] = "Mon";
} elsif ( $today eq Wed ) {
    $Nday[6] = "Tue"; $Nday[0] = "Wed"; $Nday[1] = "Thu"; $Nday[2] = "Fri"; $Nday[3] = "Sat"; $Nday[4] = "Sun"; $Nday[5] = "Mon";
} elsif ( $today eq Thu ) {
    $Nday[5] = "Tue"; $Nday[6] = "Wed"; $Nday[0] = "Thu"; $Nday[1] = "Fri"; $Nday[2] = "Sat"; $Nday[3] = "Sun"; $Nday[4] = "Mon";
} elsif ( $today eq Fri ) {
    $Nday[4] = "Tue"; $Nday[5] = "Wed"; $Nday[6] = "Thu"; $Nday[0] = "Fri"; $Nday[1] = "Sat"; $Nday[2] = "Sun"; $Nday[3] = "Mon";
} elsif ( $today eq Sat ) {
    $Nday[3] = "Tue"; $Nday[4] = "Wed"; $Nday[5] = "Thu"; $Nday[6] = "Fri"; $Nday[0] = "Sat"; $Nday[1] = "Sun"; $Nday[2] = "Mon";
} elsif ( $today eq Sun ) {
    $Nday[2] = "Tue"; $Nday[3] = "Wed"; $Nday[4] = "Thu"; $Nday[5] = "Fri"; $Nday[6] = "Sat"; $Nday[0] = "Sun"; $Nday[1] = "Mon";
} else {
    $Nday[1] = "Tue"; $Nday[2] = "Wed"; $Nday[3] = "Thu"; $Nday[4] = "Fri"; $Nday[5] = "Sat"; $Nday[6] = "Sun"; $Nday[0] = "Mon";
}
 

 $weeks = int($weeks);

for($i=1;$i<$weeks;$i++) {
    for($j=0;$j<7;$j++) {
	$Nday[$j+7*$i] = $Nday[$j];
    }
}

&StDbTJobsConnect();

my $path;
my $sql; 
my $agmlpath;
my $pth;
my $agml;

my $n_weeks = $weeks - 1;
while($n_weeks >= 0) {
    my $rn_weeks = $weeks-1-$n_weeks;
    for ($d_week = 0; $d_week <=6; $d_week++) {
	    if($d_week eq 0) {
		$day_diff = 8;
	    } else {
		$day_diff = 8-$d_week;
	    }
	$day_diff = $day_diff + 7*$n_weeks;
	$day_diff1 = 7*$n_weeks;

  $day_diff = int($day_diff);
  $day_diff1 = int($day_diff1);

  @spl = ();
  
   @spl = split(" ", $set1);
   $path = $spl[0];  
	$path =~ s(year)($Nday[$d_week]/year);

   @spl = ();
   @spl = split("/", $set1);
   $pth = $spl[0]."%" ;
   $path =~ s($spl[0])($pth)g;
   $agml = $spl[1].".AgML" ;

#	$path =~ s(/)(%)g;

 my $qupath = "%$path%";

#       $agmlpath = "%$pth".$agml."/".$Nday[$d_week]."/".$spl[2];

  $agmlpath = "%$path%";
  $agmlpath =~ s($spl[1])($agml)g;

	if ($n_weeks == 0) {

	    $sql="SELECT path, $mplotVal FROM JobStatus WHERE path LIKE ? AND avail='Y' AND jobStatus=\"Done\" AND (TO_DAYS(\"$nowdate\") -TO_DAYS(createTime)) < ? ORDER by createTime DESC LIMIT 5";

 	$cursor = $dbh->prepare($sql) || die "Cannot prepare statement: $dbh->errstr\n";
	$cursor->execute($qupath,$day_diff);

	} else {
	    $sql="SELECT path, $mplotVal FROM JobStatus WHERE path LIKE ? AND jobStatus=\"Done\" AND (TO_DAYS(\"$nowdate\") -TO_DAYS(createTime)) < ? AND (TO_DAYS(\"$nowdate\") -TO_DAYS(createTime)) > ? ORDER by createTime DESC LIMIT 5";


	$cursor = $dbh->prepare($sql) || die "Cannot prepare statement: $dbh->errstr\n";
	$cursor->execute($qupath,$day_diff, $day_diff1);

 }
	while(@fields = $cursor->fetchrow_array) {
            next if ( $fields[0] =~ /daq_sl302.icc80/) ;
	    if ($fields[0] =~ /sl302.ittf_opt/) {
		$point2[$d_week+7*$rn_weeks] = $fields[1];
		if($point2[$d_week+7*$rn_weeks] > $max_y) {
		    $max_y = $point2[$d_week+7*$rn_weeks];
		}
		if($point2[$d_week+7*$rn_weeks] < $min_y) {
		    $min_y = $point2[$d_week+7*$rn_weeks];
		}
		if ($plotVal eq "MemUsage") {
		    $point3[$d_week+7*$rn_weeks] = $fields[2];
		    if ($point3[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point3[$d_week+7*$rn_weeks];
		    }
		    if ($point3[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point3[$d_week+7*$rn_weeks];
		    }
		}
		if ($plotVal eq "RealTime_per_CPU") {
		    $point4[$d_week+7*$rn_weeks] = $fields[2];
		    if($fields[2] > 0.000001) {
# 		    $point5[$d_week+7*$rn_weeks] = $fields[1]/$fields[2];                    
        	    $point5[$d_week+7*$rn_weeks] = $point2[$d_week+7*$rn_weeks]/$point4[$d_week+7*$rn_weeks];  
		    if ($point5[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point5[$d_week+7*$rn_weeks];
		    }
		    if ($point5[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point5[$d_week+7*$rn_weeks];
		    }
		}else{
                $point5[$d_week+7*$rn_weeks] = 0
                 } 
		}
          }elsif($fields[0] =~ /sl302.ittf/) {
		$point6[$d_week+7*$rn_weeks] = $fields[1];
		if($point6[$d_week+7*$rn_weeks] > $max_y) {
		    $max_y = $point6[$d_week+7*$rn_weeks];
		}
		if($point6[$d_week+7*$rn_weeks] < $min_y) {
		    $min_y = $point6[$d_week+7*$rn_weeks];
		}
		if ($plotVal eq "MemUsage") {
		    $point7[$d_week+7*$rn_weeks] = $fields[2];
		    if ($point7[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point7[$d_week+7*$rn_weeks];
		    }
		    if ($point7[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point7[$d_week+7*$rn_weeks];
		    }
		}
		if ($plotVal eq "RealTime_per_CPU") {
		    $point8[$d_week+7*$rn_weeks] = $fields[2];
		    if($fields[2] > 0.00001) {
#		    $point9[$d_week+7*$rn_weeks] = $fields[1]/$fields[2];  
 		    $point9[$d_week+7*$rn_weeks] = $point6[$d_week+7*$rn_weeks]/$point8[$d_week+7*$rn_weeks];                  
		    if ($point9[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point9[$d_week+7*$rn_weeks];
		    }
		    if ($point9[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point9[$d_week+7*$rn_weeks];
		    }
		}else{
                $point9[$d_week+7*$rn_weeks] = 0
                 } 
		}

######
	    }

	}

############

	if ($n_weeks == 0) {

	    $sql="SELECT path, $mplotVal FROM JobStatus WHERE path LIKE ? AND avail='Y' AND jobStatus=\"Done\" AND (TO_DAYS(\"$nowdate\") -TO_DAYS(createTime)) < ? ORDER by createTime DESC LIMIT 5";

 	$cursor = $dbh->prepare($sql) || die "Cannot prepare statement: $dbh->errstr\n";
	$cursor->execute($agmlpath,$day_diff);

	} else {
	    $sql="SELECT path, $mplotVal FROM JobStatus WHERE path LIKE ? AND jobStatus=\"Done\" AND (TO_DAYS(\"$nowdate\") -TO_DAYS(createTime)) < ? AND (TO_DAYS(\"$nowdate\") -TO_DAYS(createTime)) > ? ORDER by createTime DESC LIMIT 5";

	$cursor = $dbh->prepare($sql) || die "Cannot prepare statement: $dbh->errstr\n";
	$cursor->execute($agmlpath,$day_diff, $day_diff1);

       }
	while(@fields = $cursor->fetchrow_array) {

	    if ($fields[0] =~ /sl302.ittf_opt/) {
		$point12[$d_week+7*$rn_weeks] = $fields[1];
		if($point12[$d_week+7*$rn_weeks] > $max_y) {
		    $max_y = $point12[$d_week+7*$rn_weeks];
		}
		if($point12[$d_week+7*$rn_weeks] < $min_y) {
		    $min_y = $point12[$d_week+7*$rn_weeks];
		}
		if ($plotVal eq "MemUsage") {
		    $point13[$d_week+7*$rn_weeks] = $fields[2];
		    if ($point13[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point13[$d_week+7*$rn_weeks];
		    }
		    if ($point13[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point13[$d_week+7*$rn_weeks];
		    }
		 }
#######
		if ($plotVal eq "RealTime_per_CPU") {
		    $point14[$d_week+7*$rn_weeks] = $fields[2];
		    if($fields[2] > 0.000001) {
#		    $point15[$d_week+7*$rn_weeks] = $fields[1]/$fields[2];                    
 		    $point15[$d_week+7*$rn_weeks] = $point12[$d_week+7*$rn_weeks]/$point14[$d_week+7*$rn_weeks];  
		    if ($point15[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point5[$d_week+7*$rn_weeks];
		    }
		    if ($point15[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point15[$d_week+7*$rn_weeks];
		    }
		}else{
                $point15[$d_week+7*$rn_weeks] = 0
                 } 
		}
#######
		} elsif($fields[0] =~ /sl302.ittf/) {
		$point10[$d_week+7*$rn_weeks] = $fields[1];
		if($point10[$d_week+7*$rn_weeks] > $max_y) {
		    $max_y = $point10[$d_week+7*$rn_weeks];
		}
		if($point10[$d_week+7*$rn_weeks] < $min_y) {
		    $min_y = $point10[$d_week+7*$rn_weeks];
		}
		if ($plotVal eq "MemUsage") {
		    $point11[$d_week+7*$rn_weeks] = $fields[2];
		    if ($point11[$d_week+7*$rn_weeks] > $max_y) {
			$max_y = $point11[$d_week+7*$rn_weeks];
		    }
		    if ($point11[$d_week+7*$rn_weeks] < $min_y) {
			$min_y = $point11[$d_week+7*$rn_weeks];
		    }
		}
#########
                if ($plotVal eq "RealTime_per_CPU") {
                    $point16[$d_week+7*$rn_weeks] = $fields[2];
                    if($fields[2] > 0.00001) {
#                    $point17[$d_week+7*$rn_weeks] = $fields[1]/$fields[2];
                   $point17[$d_week+7*$rn_weeks] = $point10[$d_week+7*$rn_weeks]/$point16[$d_week+7*$rn_weeks];
                    if ($point17[$d_week+7*$rn_weeks] > $max_y) {
                        $max_y = $point17[$d_week+7*$rn_weeks];
                    }
                    if ($point17[$d_week+7*$rn_weeks] < $min_y) {
                        $min_y = $point17[$d_week+7*$rn_weeks];
                    }
                }else{
                $point17[$d_week+7*$rn_weeks] = 0
                 }
                }
########               
	    }
	   }
	}
#############

    $n_weeks--;
 }

&StDbTJobsDisconnect();

@data = ();

if ($plotVal eq "MemUsage") {

#    @data = (\@Nday, \@point2,  \@point3, \@point6, \@point7 );

    @data = (\@Nday, \@point2,  \@point3, \@point6, \@point7, \@point10, \@point11, \@point12, \@point13);

    $legend[0] = "MemUsgaeFirst(ittf.optimized)";
    $legend[1] = "MemUsgaeLast(ittf.optimized)";
    $legend[2] = "MemUsgaeFirst(ittf)";
    $legend[3] = "MemUsageLast(ittf)";
    $legend[4] = "MemUsgaeFirst(ittf,AgML)";
    $legend[5] = "MemUsageLast(ittf,AgML)";
    $legend[6] = "MemUsgaeFirst(ittf.optimized,AgML)";
    $legend[7] = "MemUsageLast(ittf.optimized,AgML)";    


    $mplotVal="MemUsageFirstEvent,MemUsageLastEvent";

 }elsif($plotVal eq "RealTime_per_CPU") {

  @data = (\@Nday, \@point5,  \@point9, \@point15,  \@point17 );

    $legend[0] = "RealTime/CPU(ittf.optimized)";
    $legend[1] = "RealTime/CPU(ittf)";
    $legend[2] = "RealTime/CPU(ittf,AgML)";    
    $legend[3] = "RealTime/CPU(ittf.optimized,AgML)"; 

  $mplotVal="RealTime/CPU";

  $min_y = 0;
  $max_y = 2.7;

} else {

#    @data = (\@Nday, \@point2, \@point6 );

    @data = (\@Nday, \@point2, \@point6, \@point10, \@point12 );

    $legend[0] = "$plotVal"."(ittf.optimized)";
    $legend[1] = "$plotVal"."(ittf)";
    $legend[2] = "$plotVal"."(ittf,AgML)";    
    $legend[3] = "$plotVal"."(ittf.optimized,AgML)"; 
}


$graph = new GD::Graph::linespoints(550+50*$weeks,500);

 if ( ! $graph){
    print STDOUT $query->header(-type => 'text/plain');
    print STDOUT "Failed\n";
} else {

  my $format = $graph->export_format;
  print header("image/$format");
  binmode STDOUT;


    if( $min_y == 0) {
	$graph->set(x_label => "(0 value means job failed or data not available)");
    } else {
	# keep the min_y in the 6th ticks (6/3)
	$min_y = $min_y - ($max_y-$min_y)*2.0;
    }

    # keep the max_y in the 9th ticks
    $max_y = $max_y + ($max_y - $min_y)/9.0;

    if($max_y eq $min_y) {
	$max_y += 1;
	$min_y -= 1;
    }

    if($min_y < 0) {
	$min_y = 0;
    }

    $graph->set(#x_label => "$xlabel",
		#y_label => "$mplotVal",
		x_label_position => 0.5,
		title   => "$set1"." ($mplotVal)",
		y_tick_number => 10,
		y_min_value => $min_y,
		y_max_value => $max_y,
		y_number_format => \&y_format,
		labelclr => "lred",
		dclrs => [ qw(lblack lred lblue lgreen lpink lpurple lorange lyellow dbrown ) ],
		line_width => 2,
		markers => [ 2,3,4,5,6,7,8,9],
		marker_size => 2,
		#long_ticks => 1
		);

    $graph->set_legend(@legend);
    $graph->set_legend_font(gdMediumBoldFont);
    $graph->set_title_font(gdMediumBoldFont);
    $graph->set_x_label_font(gdMediumBoldFont);
    $graph->set_y_label_font(gdMediumBoldFont);
    $graph->set_x_axis_font(gdMediumBoldFont);
    $graph->set_y_axis_font(gdMediumBoldFont);

    print STDOUT $graph->plot(\@data)->$format();     
}


######################
sub StDbTJobsConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbTJobsDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}


##########################

sub y_format
{
    my $value = shift;
    my $ret;

    $ret = sprintf("%8.2f", $value);
}
