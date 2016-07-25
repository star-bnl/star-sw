#!/usr/bin/perl -w 

use DBI;
use strict;
use Time::Local;
use Math::BigInt;

my $day0 = Math::BigInt->new(timelocal(0,0,0,31,11,111));

my %fillstats = ();
my %statnames = ();

my %runlums = ();
my %runlums_byrun = ();

my %firsttimebyfill = ();
my %lasttimebyfill = ();
open IN,"lumis/lum_perrun_FMS-base.txt";
while (<IN>) {
  my ($run,$begin,$end,$fill,$lum,$ps,$live,$bname,$blive) = split;
#  next unless (abs($ps-1.)<1.e-5);
  
  my $ibegin = Math::BigInt->new($begin);
  my $iend = Math::BigInt->new($end);
  my $riend = Math::BigInt->new($end);
  $riend->badd($day0);
  my $ribegin = Math::BigInt->new($begin);
  $ribegin->badd($day0);
  next if (exists($runlums{$fill}{$ibegin}) && defined($runlums{$fill}{$ibegin}));
  $runlums{$fill}{$ibegin} = {ibegin=>$ibegin,end=>$iend,lum=>$lum,run=>$run,live=>$live};
  print STDERR "Using FMS-base run ${run}\n";
 
  $lasttimebyfill{$fill} = $riend unless (exists($lasttimebyfill{$fill}) && defined($lasttimebyfill{$fill}) && $lasttimebyfill{$fill}->bcmp($riend) > 0);
  $firsttimebyfill{$fill} = $ribegin unless (exists($firsttimebyfill{$fill}) &&
					    defined($firsttimebyfill{$fill}) &&
					    $firsttimebyfill{$fill}->bcmp($ribegin)<0);
}

close IN;


open IN,"lumis/lum_perrun_BHT3.txt";
while (<IN>) {
  my ($run,$begin,$end,$fill,$lum,$ps,$live,$bname,$blive) = split;
#  next unless (abs($ps-1.)<1.e-5);
  
  my $ibegin = Math::BigInt->new($begin);
  my $iend = Math::BigInt->new($end);
  my $riend = Math::BigInt->new($end);
  $riend->badd($day0);
  next if (exists($runlums{$fill}{$ibegin}) && defined($runlums{$fill}{$ibegin}) && $lum<$runlums{$fill}{$ibegin}{lum} && $runlums{$fill}{$ibegin}{live} <1);
  $runlums{$fill}{$ibegin} = {ibegin=>$ibegin,end=>$iend,lum=>$lum,run=>$run,live=>$live};
  print STDERR "Using BHT3 run ${run}\n";
  $lasttimebyfill{$fill} = $riend unless (exists($lasttimebyfill{$fill}) && defined($lasttimebyfill{$fill}) && $lasttimebyfill{$fill}->bcmp($riend) > 0);
}

my %fills = ();
open IN,"physics.txt";
while (<IN>) {
  my @s = split;
  
  $fills{$s[0]} = [Math::BigInt->new($s[1]),
		   Math::BigInt->new($s[2])];
}
my %patterntobunch = (
"103x103"=>95,
"107x107"=>97,
"107x111"=>98,
"111x107"=>98,
"111x111"=>98,
"23x23"=>22,
"53x48"=>48,
"54x54"=>52,
"95x94"=>87,
"6x6"=>6,
"108x107"=>98,
"107x109"=>98,
"109x108"=>98,
"97x94"=>92,
"97x95"=>92,
"97x97"=>92,
"49x55"=>49,
"54x56"=>52,
"55x54"=>52,
"55x56"=>52,
"56x56"=>52,
"61x61"=>56,
"65x67"=>62,
"67x66"=>62,
"68x65"=>62,
"68x68"=>62,
"74x74"=>68,
"84x84"=>78,
"93x93"=>87,
"6x6"=>6

		      );
my %bunchesbyfill = ();

my %bunchesbyrun = ();
open IN,"singlescorrections_scalers.txt";
while (<IN>) {
    my @s = split;
    $bunchesbyrun{$s[0]} = $s[2];
}
close IN;
for my $fill (sort keys %runlums) {
    for my $ibegin (sort keys %{$runlums{$fill}}) {
	my $run = $runlums{$fill}{$ibegin}{run};
	if (exists($bunchesbyrun{$run}) && defined($bunchesbyrun{$run})) {
	    $bunchesbyfill{$fill} = $bunchesbyrun{$run};
	    print STDERR "Found ",$bunchesbyrun{$run}, " bunches for run $run fill $fill\n";
	    last;
	}
    }
}
							    
my $database = "";
my $database_runlog = "";
my $database_physics = "";
my $year = 2012;
if ($year =~ /2012/) {
  $database = "onl10.starp.bnl.gov:3502";
  $database_runlog = "onl10.starp.bnl.gov:3501";
  $database_physics = "onl10.starp.bnl.gov:3606";
}
if ($year =~ /2006/) {
  $database = "dbbak.starp.bnl.gov:3405";
}
if ($year =~ /2005/) {
  $database = "dbbak.starp.bnl.gov:3404";
}
if ($year =~ /2004/) {
  $database = "dbbak.starp.bnl.gov:3403";
}
if ($year =~ /2003/) {
  $database = "dbbak.starp.bnl.gov:3402";
}

my $dbUser = '';
my $dbh = DBI->connect("DBI:mysql:Conditions_rich:${database}",$dbUser,"")
  || die "cannot connect to server $DBI::errstr\n";
my $dbr = DBI->connect("DBI:mysql:RunLog:${database_runlog}",$dbUser,"")
  || die "cannot connect to server $DBI::errstr\n";

my $dbtcu = DBI->connect("DBI:mysql:Scalers_rts:${database_runlog}",$dbUser,"");
my $dbp = undef;
if ($database_physics) {

  $dbp = DBI->connect("DBI:mysql:physics_ts_monitor:${database_physics}",$dbUser,"")
  || die "cannot connect to server $DBI::errstr\n";
}
my %physicson = ();
my %physicsoff = ();
for my $fill (keys %fills) {
  push @{$physicson{$fill}},$fills{$fill}[0];
  push @{$physicsoff{$fill}},$fills{$fill}[1];

}
if ($dbp) {
  my $query_physicson = "select fill_yellow,unix_timestamp(declaredTime) from monitor where code=520 and fill_yellow>16577";
  my $sthr_on = $dbp->prepare($query_physicson);
  $sthr_on->execute();
  %physicson = ();
  while (my $stuff = $sthr_on->fetchrow_arrayref) {
    my $fill = $stuff->[0];
    next if $fills{$fill};
    my $on = Math::BigInt->new($stuff->[1]);

    push @{$physicson{$fill}},$on;

  }
  my $query_physicsoff = "select fill_yellow,unix_timestamp(declaredTime) from monitor where code=521 and fill_yellow>16577";
  my $sthr_off = $dbp->prepare($query_physicsoff);
  $sthr_off->execute();
  %physicsoff = ();
  while (my $stuff = $sthr_off->fetchrow_arrayref) {
    my $fill = $stuff->[0];
    next if $fills{$fill};
    my $off = Math::BigInt->new($stuff->[1]);
    push @{$physicsoff{$fill}},$off;
  }
}
for my $fill (sort keys %physicson) {
  next unless ($physicsoff{$fill});
#  next if ($fills{$fill});
    # Make it only one
  my @son = sort{$a->bcmp($b)} @{$physicson{$fill}};
  my @soff = sort {$a->bcmp($b)} @{$physicsoff{$fill}};
  my $on = $son[0];
  my $off = undef;
 LOOPOFF: for my $ioff (@soff) {
     
    unless ($ioff->bcmp($lasttimebyfill{$fill})<0) {
      $off=$ioff;
      
      last LOOPOFF;
    }
  }
  $off = $soff[0] unless (defined($off));

  if (defined($off) && defined($lasttimebyfill{$fill}) && $off->bcmp($lasttimebyfill{$fill})<0) {
      print STDERR "Last run ends at ",scalar(localtime($lasttimebyfill{$fill}->numify)),", later than ",scalar(localtime($off->numify))," shifting\n";
      $off = $lasttimebyfill{$fill};
  }
  if (defined($on)&&defined($firsttimebyfill{$fill})&&$on->bcmp($firsttimebyfill{$fill})>0 ) {

      print STDERR "First run starts at ",scalar(localtime($firsttimebyfill{$fill}->numify)),", earlier than ",scalar(localtime($on->numify))," shifting\n";
      $on = $firsttimebyfill{$fill};
  }

#    my $ion = $on->numify();
  $fills{$fill} = [$on,$off];

  print STDERR "Fill $fill started ", scalar(localtime($fills{$fill}[0]->numify())), " ended ", scalar(localtime($fills{$fill}[1]->numify()))," Last run ended ",scalar(localtime($lasttimebyfill{$fill})),"\n";

}


    
    


open IN,"bunchesbyfill.txt";
while (<IN>) {
    my ($fill,$pat) = split;
    if ($patterntobunch{$pat}) {
	$bunchesbyfill{$fill} = $patterntobunch{$pat};
    }
    else {
	print STDERR "No bunches for $pat, fill $fill.  Taking 98\n";
	$bunchesbyfill{$fill} = 98;
    }
}

my %lasersbyfill = ();
# my $query_lasers = "select a.runNumber,a.startRunTime,a.endRunTime,a.glbSetupName,b.rtsStatus,b.shiftLeaderStatus from runDescriptor a, runStatus b, detectorSet c where a.runNumber=b.runNumber and b.runNumber=c.runNumber and c.detectorId=20 order by a.runNumber";
# my $sth_lasers = $dbr->prepare($query_lasers);
# my @lasers = ();
# $sth_lasers->execute();
# my $startlaser = undef;
# my $endlaser = undef;
# my $islaser = 0;
# while (my $stuff = $sth_lasers->fetchrow_arrayref) {
#     my $run = $stuff->[0];
#     my $begin = Math::BigInt->new($stuff->[1]);
#     my $end = Math::BigInt->new($stuff->[2]);
#     my $glb = $stuff->[3];
#     my $rts = $stuff->[4];
#     my $shift = $stuff->[5];
#     if ($glb =~ /roduction/i && !$islaser&&!$rts) {
# 	$startlaser = $end;
# 	next;
#     }
#     if ($glb =~/laser/i) {
# 	$islaser = 1;
# 	next;
#     }
#     if ($glb =~/roduction/i && $islaser && !$rts) {
# 	push @lasers,[$startlaser,$begin];
# 	$islaser = 0;
#     }
# }


#  my %lasersbyfill = ();

# my $lastlaser = 0;
# for my $fill (sort keys %fills) {
#     for my $ilaser($lastlaser..$#lasers) {
# 	my $islaser = $lasers[$ilaser];
# 	if (!defined($islaser->[0]) &&
# 	    ($islaser->[1]->bcmp($fills{$fill}->[0])>=0) &&
# 	    ($islaser->[1]->bcmp($fills{$fill}->[1])<=0)) {
# 	    $islaser->[0] = $fills{$fill}->[0];
# 	}
# 	next unless (defined($islaser->[0]));
		     
# 	if ($islaser->[0]->bcmp($fills{$fill}->[0])>=0 
# 	    &&
# 	    $islaser->[0]->bcmp($fills{$fill}->[1])<=0) 
# 	{
# 	    my $newlaser = $islaser;
# 	    if (($islaser->[1]->bcmp($fills{$fill}->[1])>0)) {
# 		$newlaser->[1] = $fills{$fill}->[1];
# 	    }
# 	    $lastlaser = $ilaser+1;
# 	    push @{$lasersbyfill{$fill}}, $newlaser;
#  	    print STDOUT "Found laser (",
#  	    scalar(localtime($newlaser->[0]->bstr)),",",
#  	    scalar(localtime($newlaser->[1]->bstr)),
#  	    ") in fill $fill (",
#  	    scalar(localtime($fills{$fill}->[0]->bstr)),",",
#  	    scalar(localtime($fills{$fill}->[1]->bstr)),
#  	    ")\n";
# 	}
	
#     }
# }
# for my $fill (sort keys %lasersbyfill) {
#     open OUT,">lasers_fill${fill}.txt";
#     print OUT scalar(@{$lasersbyfill{$fill}}),"\n";
#     for my $laser (@{$lasersbyfill{$fill}}) {
# 	my $rbeg = ($laser->[0]->bsub($fills{$fill}->[0]))->numify()/60./60.;
# 	my $rend = ($laser->[1]->bsub($fills{$fill}->[0]))->numify()/60./60.;
# 	print OUT $rbeg," ",$rend,"\n";
#     }
# }

#my $query = "select unix_timestamp(beginTime),rs15,rs12,rs13 from richScalar where unix_timestamp(beginTime)>? and unix_timestamp(beginTime)<? order by beginTime";
#my $query = "select unix_timestamp(beginTime),rs8,rs6,rs7 from richScalar where unix_timestamp(beginTime)>? and unix_timestamp(beginTime)<? order by beginTime";
#my $query = "select unix_timestamp(beginTime),rs11,rs10,rs9 from richScalar where unix_timestamp(beginTime)>? and unix_timestamp(beginTime)<? order by beginTime";
#my $query = "select unix_timestamp(beginTime),rs3,rs1,rs2 from richScalar where unix_timestamp(beginTime)>? and unix_timestamp(beginTime)<? order by beginTime";
# ZDC Nokil
my $query = "select unix_timestamp(beginTime),rs15,rs12,rs13 from richScalar where unix_timestamp(beginTime)>? and unix_timestamp(beginTime)<? order by beginTime";

my $sth = $dbh->prepare($query);
#my $crosssection = 25e9; # VPD? 
my $crosssection = 2.48e9; #ZDC from van der Meer scan, early March 2012
my $fullrun_first_lum = 0;
my $fullrun_last_lum = 0;
my $fullrun_first_hours = 0;
my $fullrun_last_hours = 0;

my $fullrun_samp_lum = 0;
my $fullrun_del_lum = 0;
my $fullrun_laser_lum = 0;
my $nfills = 0;
my $fullrun_samp_hours = 0;
my $fullrun_del_hours = 0;
my $fullrun_laser_hours = 0;

open ROOT,"| root -b";
open HTML,">plots.html";
print HTML <<EOF;
<html>
<body bgcolor="white">
<table border=1>
EOF
my %lumbyfill = ();
my %sampbyfill = ();
my %laserbyfill = ();


my $queryidx = "select daqTriggerId from l0TriggerSet where runNumber=? and (name='BHT3' or name='FMS-base')";
my $querylive = "select time,tag,type,counter from tcuCounters where run=? and idx=?";
my $sthidx = $dbr->prepare($queryidx);
my $sthlive = $dbtcu->prepare($querylive);

open OUTLIVE,">tcu_live_byrun.txt";
for my $fill (sort keys %fills) {

  my $livebylum= 0;
  my $sumlum = 0;
  for my $ibegin (sort keys %{$runlums{$fill}}) {
    my $run = $runlums{$fill}{$ibegin}{"run"};
    print STDERR "Getting TCU live for run ${run}\n";
    $sthidx->execute($run);
    my $idx = 4;
    while (my $stuff = $sthidx->fetchrow_arrayref) {
      $idx=$stuff->[0];
    }
    
    $sthlive->execute($run,$idx);
# #    my $sumpresented = 0;
    # #    my $sumlive = 0;
    my $start = Math::BigInt->new(0);
    my $end = Math::BigInt->new(0);
    while (my $stuff=$sthlive->fetchrow_arrayref) {
      my $time = Math::BigInt->new($stuff->[0]);
      $time->bsub($day0);
      
      my $tag = $stuff->[1];
      my $type = $stuff->[2];
      my $counter = $stuff->[3];
      if ($tag == 1) {
 	if ($time->bcmp($runlums{$fill}{$ibegin}{"ibegin"}) > 0 &&
	    $time->bcmp($runlums{$fill}{$ibegin}{"end"}) < 0 ) {
 	  $runlums{$fill}{$ibegin}{"ibegin"} = $time;
 	}
 	next;
      }
      if ($tag == 2) {
 	if ($time->bcmp($runlums{$fill}{$ibegin}{"end"}) < 0 && 
	    $time->bcmp($runlums{$fill}{$ibegin}{"ibegin"})>0) {
 	  $runlums{$fill}{$ibegin}{"end"} = $time;
 	}
	
      
# #	if ($tag == 2 && $type==3) {
# #	  $sumpresented += $counter;
# #	}
# #	if ($tag == 2 && $type==4) {
# #	  $sumlive += $counter;
# #	}
	#       }
      }
    }
    #    my $live = ($sumpresented>0) ? $sumlive/$sumpresented : 0;
    #    $totpresented += $sumpresented;
    #    $totlive += $sumlive;
    #    $runlums{$fill}{$ibegin}{"live"} = $live;
    my $live = $runlums{$fill}{$ibegin}{"live"};
    $livebylum += $live*$runlums{$fill}{$ibegin}{"lum"};
    my $pbeg = Math::BigInt->new($runlums{$fill}{$ibegin}{"ibegin"});
    #    $pbeg->bsub($day0);
    my $pend = Math::BigInt->new($runlums{$fill}{$ibegin}{"end"});
    #    $pend->bsub($day0);
    
    print OUTLIVE
      $pbeg->numify()/(24*60*60), " $live\n";
    
  }

  open OUT,">delivered_fill${fill}.txt";
  my $sum = 0;
  $sth->execute($fills{$fill}[0],$fills{$fill}[1]);
  my $started = Math::BigInt->new(0);
  
  while (my $stuff = $sth->fetchrow_arrayref) {
    my $start = Math::BigInt->new($stuff->[0]);
    
    if ($started>0) {
      my $diff = $start - $started;
      my $rdiff = $diff->numify();
      my $n12 = $stuff->[1];
      my $n1 = $stuff->[2];
      my $n2 = $stuff->[3];
#       # First correct for 1 us deadtime
#        my $killersing  = 1.3e-6;
#        my $killercoin = 1.3e-6;
#        if ($fill>11690) {
#  	  $killersing = 0.4e-6;
#  	  $killercoin = 1.e-6;
#        }
#        my $an = $n12*($killercoin);
#       my $an12 = $n12*(1.+$an+3./2.*($an**2)
# 		       + 8./3.*($an**3)
# 		       + 125./4.*($an**4)
# 		       + 54./5.*($an**5)
# 		       + 16807./720.*($an**6)
# 		       + 16384/315*($an**7)
# 		       + 531441./4480.*($an**8)
# 		       + 156250./567.*($an**9));
# #      my $an12 = $n12/(1.-$an);

#       $an = $n1*($killersing);
#       my $an1 = $n1*(1.+$an+3./2.*($an**2)
# 		       + 8./3.*($an**3)
# 		       + 125./4.*($an**4)
# 		       + 54./5.*($an**5)
# 		       + 16807./720.*($an**6)
# 		       + 16384/315*($an**7)
# 		       + 531441./4480.*($an**8)
# 		       + 156250./567.*($an**9));
# #      my $an1 = $n1/(1.-$an);

#       $an = $n2*($killersing);

#       my $an2 = $n2*(1.+$an+3./2.*($an**2)
# 		       + 8./3.*($an**3)
# 		       + 125./4.*($an**4)
# 		       + 54./5.*($an**5)
# 		       + 16807./720.*($an**6)
# 		       + 16384/315*($an**7)
# 		       + 531441./4480.*($an**8)
# 		       + 156250./567.*($an**9));
# # #      my $an2 = $n2/(1.-$an);


      my $an1 = $n1; #East, no killer
      my $an2 = $n2; #West, no killer
      my $an12 = $n12;


      my $nbunches = $bunchesbyfill{$fill};
       unless ($nbunches) {
 	  print STDERR "No bunches for fill $fill.  Taking 98\n";
 	  $nbunches = 98.;
       }
       my $nbc = $nbunches/120.*9.383e6;

       my $test = 1.0-($an12-$an1*$an2/$nbc)/($nbc + $an12-$an1-$an2);
#       my $singlescorr = ($stuff->[1]-$stuff->[2]*$stuff->[3]/9.383e6);
      #my $singlescorr = 0;
      my $singlescorr = 0;
#Sanity check for magnet trips
       if ($test>0 && $n12>0 && $n1*$n2/$nbc/$n12 < 0.7 &&
	   ($fill<16622 || $fill>=16632 || $n12<50000) ) {
	   
 	  $singlescorr = -$nbc*log($test);
#	   $singlescorr = $an12;
       }
       else {
 	  print STDERR "Log died. Fill $fill ",scalar(localtime($stuff->[0]))," $singlescorr $test $n12 $n1 $n2 $an12 $an1 $an2 \n";
       }
      #$an12 = $singlescorr;
#      my $singlescorr = $n12-$an1*$an2/$nbc;
      if ($singlescorr<0) {
	$singlescorr=0;
      }
#      my $singlescorr = $n12;
#      my $an12 = $singlescorr;
      my $lum = 0;
      if ($fill < 16622 || $fill >= 16632) {
	  $crosssection=2.48e9;
      }
      else {
	  $crosssection=5.48e7;
      }
      $lum = ($singlescorr)*$rdiff/$crosssection if ($singlescorr>0);
      #$lum = ($n12-$n1*$n2/$nbc)*$rdiff/$crosssection;
      my $rstart = $started-$day0;
      my $rend = $start-$day0;
      my $printstart = $rstart->numify();
      my $end = $rend->numify();
# Kludge for different cross section
      my $correction = ($n12>0)?$singlescorr/$n12:1.;
#      my $correction = 1;
     if ($correction<0) {
	$correction = 0;
      }
#      if ($n12>0 && $an12>0) {
#      	 $lumbyfill{$fill}{$printstart} = {ibegin=>$rstart,end=>$rend,lum=>$lum,corr_killer=>$singlescorr/$n12,corr_nokiller=>$singlescorr/$an12,raw_killer=>$n12};
      	 $lumbyfill{$fill}{$printstart} = {ibegin=>$rstart,end=>$rend,lum=>$lum,corr_killer=>$correction,corr_nokiller=>$correction,raw_killer=>$n12,corr_east=>$an1,corr_west=>$an2};

#        }
#       else {
# 	$lumbyfill{$fill}{$printstart} = {ibegin=>$rstart,end=>$rend,lum=>$lum,corr_killer=>-999,corr_nokiller=>-999,raw_killer=>$n12};

#       }	 
      $sum += $lum;
      my $itinfill = ($start-$fills{$fill}[0]);
      my $tinfill = $itinfill->numify()/60./60.;
      print OUT $tinfill," ",$sum,"\n";
    }
    $started = $start;
  }
  my %fracs=();
  for my $pbegin (sort {$a<=>$b} keys %{$lumbyfill{$fill}} ) {
    my $begin = $lumbyfill{$fill}{$pbegin}{ibegin}->numify();
    my $isin = 0;
    my $end = $lumbyfill{$fill}{$pbegin}{end}->numify();
    for my $rpbegin (sort keys %{$runlums{$fill}}) {
      my $rbegin = $runlums{$fill}{$rpbegin}{ibegin}->numify();
      my $rend = $runlums{$fill}{$rpbegin}{end}->numify();
      if ($end<$rbegin) {
	next;
      }
      if ($begin>$rend) {
	next;
      }
      if ($begin>$rbegin && $end < $rend) {
	$fracs{$rbegin}{$begin} = 1.;
	next;
      }
      if ($end < $rend && $rbegin>$begin) {
	$fracs{$rbegin}{$begin} = ($end-$rbegin)/($end-$begin);
	next;
      }
      if ($end>$rend && $rbegin<$begin) {
	$fracs{$rbegin}{$begin} = ($rend-$begin)/($end-$begin);
	next;
      }
      if ($end>$rend && $rbegin>$begin) {
	$fracs{$rbegin}{$begin} = ($rend-$rbegin)/($end-$begin);
	next;
      }
    }
  }
  # Now sum it by run
  my %delbyrun = ();
  open CORR,">corrections_fill${fill}.txt";
  my $totalon = 0;
  my $samptime = Math::BigInt->new(0);
  my @keys = keys %{$runlums{$fill}};
  my $firstsamp = $fills{$fill}[1];
  my $lastsamp = $fills{$fill}[1];
  my $firstlum = undef;
  for my $start (sort {$a<=>$b} keys %{$lumbyfill{$fill}}) {
      $firstlum = $lumbyfill{$fill}{$start}{lum};
  }     
  my $lastlum = 0;
 
  if (@keys>0) {
    my @sortkeys = sort {$a<=>$b} @keys;
    $firstsamp = undef;
    $lastsamp = undef;
    $firstsamp = $runlums{$fill}{$sortkeys[0]}{ibegin}+$day0 if (@keys);
    $lastsamp = $runlums{$fill}{$sortkeys[$#keys]}{end}+$day0 if (@keys);
    for my $rbegin (sort {$a<=>$b} keys %{$runlums{$fill}}) {
      my $del = 0;
      $samptime += $runlums{$fill}{$rbegin}{end}-$runlums{$fill}{$rbegin}{ibegin};
      
      my $corrsum_killer = 0;
      my $corrsum_nokiller = 0;
      my $rawsum_killer = 0;
      my $corrsum_east = 0;
      my $corrsum_west = 0;
      my $avnum = 0;
      for my $begin (sort {$a<=>$b} keys %{$lumbyfill{$fill}}) {
	
	next unless (exists($fracs{$rbegin}{$begin}) &&
		     defined($fracs{$rbegin}{$begin}));
	$del += $fracs{$rbegin}{$begin}*$lumbyfill{$fill}{$begin}{lum};
	$avnum += $fracs{$rbegin}{$begin};
	$corrsum_killer += $fracs{$rbegin}{$begin}*$lumbyfill{$fill}{$begin}{corr_killer};
	$corrsum_nokiller += $fracs{$rbegin}{$begin}*$lumbyfill{$fill}{$begin}{corr_nokiller};
	$rawsum_killer += $fracs{$rbegin}{$begin}*$lumbyfill{$fill}{$begin}{raw_killer};
	$corrsum_east += $fracs{$rbegin}{$begin}*$lumbyfill{$fill}{$begin}{corr_east};
	$corrsum_west += $fracs{$rbegin}{$begin}*$lumbyfill{$fill}{$begin}{corr_west};
	
      }
      my $corrav_nokiller = 1;
      my $corrav_killer = 1;
      my $rawav_killer = 1;
      my $corrav_east = 0;
      my $corrav_west = 0;
      if ($avnum>0) {
	$corrav_killer = $corrsum_killer/$avnum;
	$corrav_nokiller = $corrsum_nokiller/$avnum;
	$rawav_killer = $rawsum_killer/$avnum;
	$corrav_east = $corrsum_east/$avnum;
	$corrav_west = $corrsum_west/$avnum;
	my $run = $runlums{$fill}{$rbegin}{run};
	print CORR "$run $rawav_killer $corrav_killer $corrav_nokiller $corrav_east $corrav_west\n";
      }
      
      $delbyrun{$rbegin} = $del;
      # Singles correct the lumi.
#      if ($runlums{$fill}{$rbegin}{lum}) {
#	my $correction = $corrav_nokiller;
#	$runlums{$fill}{$rbegin}{lum}*=$correction;
#      }
      $totalon += $del;
    }
    $firstlum = 0;
    
    for my $begin (sort {$a<=>$b} keys %{$lumbyfill{$fill}}) {
      if (exists($fracs{$sortkeys[0]}{$begin}) &&
	  defined($fracs{$sortkeys[0]}{$begin})
	 ) {
	$firstlum += (1.-$fracs{$sortkeys[0]}{$begin})*$lumbyfill{$fill}{$begin}{lum};
	last;
      }
      $firstlum += $lumbyfill{$fill}{$begin}{lum};
    }
# Tweak it if the beginning of the first run is before the beginning of the fill
    {
      my $startfill = Math::BigInt->new($fills{$fill}[0]->numify());
      $startfill->bsub($day0);
      if ($runlums{$fill}{$sortkeys[0]}{ibegin}->bcmp($startfill) <0) {
	$firstlum=0;
      }
    }
    $lastlum = 0;
    for my $begin (sort {$b<=>$a} keys %{$lumbyfill{$fill}}) {
      if (exists($fracs{$sortkeys[$#sortkeys]}{$begin}) &&
	  defined($fracs{$sortkeys[$#sortkeys]}{$begin}) 
	 ) {
	$lastlum += (1.-$fracs{$sortkeys[$#sortkeys]}{$begin})*$lumbyfill{$fill}{$begin}{lum};
	last;
      }
      $lastlum += $lumbyfill{$fill}{$begin}{lum};
    }
# Tweak it if the end of the last run is after the end of the fill
    {
      my $endfill = Math::BigInt->new($fills{$fill}[1]->numify());
      $endfill->bsub($day0);
      if ($runlums{$fill}{$sortkeys[$#sortkeys]}{end}->bcmp($endfill) >0) {
	$lastlum = 0;
      }
    }

  }
  open OUT,">sampled_fill${fill}.txt"; 
  my $sumsamp = 0;
  for my $pbegin (sort {$a<=>$b} keys %{$runlums{$fill}}) {
    my $begin = $runlums{$fill}{$pbegin}{ibegin};
    my $itprint = ($begin+$day0-$fills{$fill}[0]);
    my $tprint = $itprint->numify()/60./60.;
    print OUT $tprint," ",$sumsamp,"\n";
    $sumsamp += $runlums{$fill}{$pbegin}{lum};
    $itprint = ($runlums{$fill}{$pbegin}{end}+$day0-$fills{$fill}[0]);
    $tprint = $itprint->numify()/60./60.;
    print OUT $tprint," ",$sumsamp,"\n";
  }
  # Now parse out fractions while in and while not
  close OUT;
  #next unless ($sumsamp > 0);
  $sampbyfill{$fill} = $sumsamp;
  
# Now parse out lasers

  my $timelaser = 0;
  my $lumlaser = 0;
 
  open OUT,">lasers_fill${fill}.txt";
  if (defined($lasersbyfill{$fill})) {
      print OUT scalar(@{$lasersbyfill{$fill}}),"\n";
  }
  
  for my $rpbegin (@{$lasersbyfill{$fill}}) {
      my $runninglumend = 0;
      my $runninglumbegin = 0;
      my $rbegin = ($rpbegin->[0]-$fills{$fill}->[0])->numify();
      my $rend = ($rpbegin->[1]-$fills{$fill}->[0])->numify();
      for my $pbegin (sort {$a<=>$b} keys %{$lumbyfill{$fill}} ) {
	  my $begin = ($lumbyfill{$fill}{$pbegin}{ibegin}+$day0-$fills{$fill}->[0])->numify();
	  my $isin = 0;
	  my $end = ($lumbyfill{$fill}{$pbegin}{end}+$day0-$fills{$fill}->[0])->numify();
	  my $lum = $lumbyfill{$fill}{$pbegin}{lum};
	  $runninglumend += $lum;

	  my $frac = 0;
	  if ($end<$rbegin) {
	      $runninglumbegin += $lum;
	      next;
	  }
	  if ($begin>$rend) {
	      last;
	  }
	  if ($begin>$rbegin && $end < $rend) {
	      $frac = 1.;

	  }
	  if ($end < $rend && $rbegin>$begin) {
	      $frac = ($end-$rbegin)/($end-$begin);
	      
	  }
	  if ($end>$rend && $rbegin<$begin) {
	      $frac= ($rend-$begin)/($end-$begin);
	  }
	  if ($end>$rend && $rbegin>$begin) {
	      $frac = ($rend-$rbegin)/($end-$begin);
	  }
	  $timelaser += ($end-$begin)*$frac;
	  $lumlaser += $lum*$frac;

      }
      print OUT $rbegin/60./60.," ",$rend/60./60.," ",$runninglumbegin," ",$runninglumend,"\n";
  }
  close OUT;
  unless ($sum>0) {
    print "Fill: $fill Delivered: $sum\n";
    next;
  }
  #next unless ($sumsamp>0 && $sum>0 && $totalon>0);
  select STDOUT;
  my $totlivefrac = ($sumsamp>0) ? $livebylum/$sumsamp : 0;
  printf qq(Fill: %s Delivered: %5.3f Sampled Frac: %5.3f Del frac taking data:%5.3f TCU Live %5.3f\n),$fill,$sum,$sumsamp/$sum,$totalon/$sum,$totlivefrac; 
  ++$nfills;
  my $deltime = $fills{$fill}[1]-$fills{$fill}[0]; 
  my $lostfirst = (defined($firstsamp))?$firstsamp-$fills{$fill}[0]:$fills{$fill}[1]-$fills{$fill}[0];
  my $lostlast = defined($lastsamp)?$fills{$fill}[1]-$lastsamp:0;
  $fullrun_del_lum += $sum;
  $fullrun_samp_lum += $sumsamp;
  $fullrun_laser_lum += $lumlaser;
  $fullrun_first_lum += $firstlum;
  $fullrun_last_lum += $lastlum;
  $fullrun_del_hours += $deltime->numify()/60./60.;
  $fullrun_samp_hours += $samptime->numify()/60./60.;
  $fullrun_laser_hours += $timelaser/60./60.;
  $fullrun_first_hours += $lostfirst->numify()/60./60.;
  $fullrun_last_hours += $lostlast->numify()/60./60.;


  open OUT1,">live_byrun_fill${fill}_begin.txt";
  open OUT2,">live_byrun_fill${fill}_end.txt";
  open OUTTCU1,">tculive_byrun_fill${fill}_begin.txt";
  open OUTTCU2,">tculive_byrun_fill${fill}_end.txt";
  for my $begin (sort keys %{$runlums{$fill}}) {
    my $rbegin = $runlums{$fill}{$begin}{ibegin};
    my $rend = $runlums{$fill}{$begin}{end};

    my $dellum = $delbyrun{$begin};
    my $itprint = ($rbegin+$day0-$fills{$fill}[0]);
    my $tprint = $itprint->numify()/60./60.;
    next unless ($dellum>0);
    print OUT1 $tprint," ",$runlums{$fill}{$begin}{lum}/$dellum,"\n";
    print OUTTCU1 $tprint," ",$runlums{$fill}{$begin}{live},"\n";
    $itprint = ($rend+$day0-$fills{$fill}[0]);
    $tprint = $itprint->numify()/60./60.;
    print OUT2 $tprint," ",$runlums{$fill}{$begin}{lum}/$dellum,"\n";
    print OUTTCU2 $tprint," ",$runlums{$fill}{$begin}{live},"\n";

  }
  close OUT1;
  close OUT2;
  close OUTTCU1;
  close OUTTCU2;
  if ($fill>=11975 && $totalon>0) {
  print ROOT ".x plotEff.C(${fill});\n";
  print HTML<<EOF;
<tr>
<td> 
<table>
<tr><td> <a name=${fill}> Fill ${fill}</td></tr> 
EOF

$fillstats{Started}{$fill} = $fills{$fill}[0];
#  $statnames{Started} = "Started";

print HTML " <tr><td>Started ",scalar(localtime($fills{$fill}[0])),"</td></tr>\n";
#  $statnames{Ended} = "Ended";

$fillstats{Ended}{$fill} = $fills{$fill}[0];
print HTML "<tr><td> Ended ",scalar(localtime($fills{$fill}[1])),"</td></tr>\n";


printf HTML qq(<tr><td> %5.1f Hours</td></tr> \n),$deltime->numify()/60./60.;

  $fillstats{Delivered}{$fill} = $sum;
  $statnames{Delivered} = "Delivered Luminosity [pb^{-1}]";

printf HTML qq(<tr> <td>Total delivered: %5.3f pb^-1 </td></tr>\n),$sum;

  $statnames{SampledFrac} = "L Sampled Fraction";

# Kludge: tweak by TCU Live/live
  $fillstats{SampledFrac}{$fill} = $sumsamp/$sum;
  $fillstats{SampledFrac}{$fill} *= $totlivefrac/($sumsamp/$totalon);
      printf HTML qq(<tr><td>Sampled Fraction: %5.3f</td></tr> \n),$fillstats{SampledFrac}{$fill};
  printf HTML qq(<tr><td> reduced by average TCULive/Live: %5.3f </td></tr> \n),$totlivefrac/($sumsamp/$totalon);

printf HTML qq(<tr><td>Sampled Fraction: %5.3f</td></tr> \n),$sumsamp/$sum;

  $fillstats{Ldev_data}{$fill} = $totalon/$sum;
  $statnames{Ldev_data} = "Fraction of L delivered while taking data";
printf HTML qq(<tr><td>Fraction of L delivered while taking data: %5.3f</td></tr>\n),$totalon/$sum;
  $fillstats{Hdev_data}{$fill} = $samptime->numify()/$deltime->numify();
  $statnames{Hdev_data} = "Fraction of Hours delivered while taking data";

printf HTML qq(<tr><td>Fraction of hours delivered while taking data: %5.3f </td></tr>\n),$samptime->numify()/$deltime->numify();

  $fillstats{Hlost_firstrun}{$fill} = $lostfirst->numify()/$deltime->numify();
  $statnames{Hlost_firstrun} = "Fraction of hours lost before first run";
  $fillstats{Hlost_firstrun_abs}{$fill} = $lostfirst->numify()/60.;
  $statnames{Hlost_firstrun_abs} = "Minutes lost before first run";

printf HTML qq(<tr><td>Minutes lost before first run: %5.1f Frac: %5.3f </td></tr>\n),$lostfirst->numify()/60.,$lostfirst->numify()/$deltime->numify();

  $fillstats{Hlost_lastrun}{$fill} = $lostlast->numify()/$deltime->numify();
  $statnames{Hlost_lastrun} = "Fraction of hours lost after last run";
  $fillstats{Hlost_lastrun_abs}{$fill} = $lostlast->numify()/60.;
  $statnames{Hlost_lastrun_abs} = "Minutes lost after last run";

printf HTML qq(<tr><td>Minutes lost after last run: %5.1f Frac: %5.3f </td></tr>\n),$lostlast->numify()/60.,$lostlast->numify()/$deltime->numify();
  $fillstats{Lumfrac_firstrun}{$fill} = $firstlum/$sum;
  $statnames{Lumfrac_firstrun} = "L Fraction lost before first run";

printf HTML qq(<tr><td>Luminosity fraction lost before first run: %5.3f</td></tr>\n),$firstlum/$sum;
  
  $fillstats{Lumfrac_lastrun}{$fill} = $lastlum/$sum;
  $statnames{Lumfrac_lastrun} = "L Fraction lost after last run";

printf HTML qq(<tr><td>Luminosity fraction lost after last run: %5.3f</td></tr>\n),$lastlum/$sum;

  
    $fillstats{LiveAverage}{$fill} = $sumsamp/$totalon;
  
    $statnames{LiveAverage} = "Average Live Time while taking data";

printf HTML qq(<tr><td>Average Live Time while taking data: %5.3f </td></tr>\n),$sumsamp/$totalon;
  
  $fillstats{LiveTCU}{$fill} = $totlivefrac;
  $statnames{LiveTCU} = "Live Time from TCU Counters while taking data";
  printf HTML qq(<tr><td>Live Time from TCU Counters while taking data: %5.3f </td></tr>\n),$totlivefrac;

  printf HTML qq(<tr><td>Luminosity fraction lost in lasers: %5.3f</td></tr>\n),$lumlaser/$sum;
  printf HTML qq(<tr><td>Hours lost in lasers: %5.1f Frac: %5.3f </td></tr>\n),$timelaser/60./60.,$timelaser/$deltime->numify();
  print HTML<<EOF;
</table>
</td>
<td> <a href="efficiency_fill${fill}.pdf"> <img src=efficiency_fill${fill}.png> </a></td>

</tr>
EOF

}
}
print HTML "</table><table border=0>\n";
close OUTLIVE;
my %statav;
for my $stat (sort {$a cmp $b} keys %statnames) {
    my $statsum = 0;
    my $delsum = 0;
    open OUT,">${stat}_byfill.txt";
    for my $fill (sort keys %{$fillstats{$stat}}) {
	print OUT $fill," ",$fillstats{$stat}{$fill},"\n";
	if ($fillstats{Delivered}{$fill}) {
	    $statsum += $fillstats{Delivered}{$fill}*$fillstats{$stat}{$fill};
	    $delsum += $fillstats{Delivered}{$fill};
	}
    }
    my $statav = $statsum/$delsum;
    $statav{$stat} = $statav;
    close OUT;
    my $statname = $statnames{$stat};
    print HTML "<tr><td><a name=\"${stat}\"> ${statname}<br>Average: ${statav}<br>Weighted by Delivered L</td><td> <a href=\"${stat}_byfill.pdf\"> <img src=\"${stat}_byfill.png\"> </a></td></tr>\n";
    print STDERR ".x plotStatByFill.C(\"${stat}_byfill\",\"${statname}\",${statav});\n";
    print ROOT ".x plotStatByFill.C(\"${stat}_byfill\",\"${statname}\",${statav});\n";
}
print HTML "</table>\n";
print ROOT ".x plotDay.C(\"tcu_live_byrun\",\"TCU Live\",",$statav{LiveTCU},");\n";
print HTML "<table border=0>\n";
print HTML "<tr><td><a name=\"TCULive_byday\"> Live Time from TCU vs. Day<br>Average: ",$statav{LiveTCU},"</td><td><a href=\"tcu_live_byrun.pdf\"><img src=\"tcu_live_byrun.png\"></td></tr>\n";
print HTML "</table>\n";


print ROOT ".q";
close ROOT;
system "convert -antialias -crop 0x0 -geometry 600x600 -density 300x300 tcu_live_byrun.eps tcu_live_byrun.png";
system "ps2pdf -DEPSCrop tcu_live_byrun.eps";
unlink "tcu_live_byrun.eps";
for my $stat (keys %statnames) {
    if (-f "${stat}_byfill.eps") {
	  system "convert -antialias -crop 0x0 -geometry 600x600 -density 300x300 ${stat}_byfill.eps ${stat}_byfill.png";
	  system "ps2pdf -DEPSCrop ${stat}_byfill.eps";
	  unlink "${stat}_byfill.eps";
      }
}
printf HTML qq(Luminosity: Delivered %5.3f Sampled %5.3f Laser %5.3f First %5.3f Last %5.3f\n),$fullrun_del_lum,$fullrun_samp_lum,$fullrun_laser_lum,$fullrun_first_lum,$fullrun_last_lum;
printf HTML qq(Hours: Delivered %5.1f Sampled %5.1f Laser %5.1f First %5.1f Last %5.1f\n),$fullrun_del_hours,$fullrun_samp_hours,$fullrun_laser_hours,$fullrun_first_hours,$fullrun_last_hours;

print HTML "</body></html>\n";
close HTML;
print STDOUT "Totals: \n";
printf STDOUT qq(Luminosity: Delivered %5.3f Sampled %5.3f Laser %5.3f First %5.3f Last %5.3f\n),$fullrun_del_lum,$fullrun_samp_lum,$fullrun_laser_lum,$fullrun_first_lum,$fullrun_last_lum;
printf STDOUT qq(Hours: Delivered %5.1f Sampled %5.1f Laser %5.1f First %5.1f Last %5.1f\n),$fullrun_del_hours,$fullrun_samp_hours,$fullrun_laser_hours,$fullrun_first_hours,$fullrun_last_hours;

for my $fill (keys %fills) {
#  next unless ($sampbyfill{$fill});
  if (-f "efficiency_fill${fill}.eps") {
      unless (-f "efficiency_fill${fill}.png") {
	  system "convert -antialias -crop 0x0 -geometry 600x600 -density 300x300 efficiency_fill${fill}.eps efficiency_fill${fill}.png";
      }
      unless (-f "efficiency_fill${fill}.pdf") {
	  system "ps2pdf -DEPSCrop efficiency_fill${fill}.eps";
      }
      system "rm efficiency_fill${fill}.eps";
  }
}

