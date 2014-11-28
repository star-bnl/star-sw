#!/usr/bin/perl -w

use DBI;
use Time::Local;
use strict;
# Connect up to RunLog
my $conrun = join(":","DBI:mysql","RunLog","onldb.starp.bnl.gov:3501");
my $dbhrun = DBI->connect($conrun,"","");
unless (defined($dbhrun)) {
  print STDERR "Bad: $DBI::errstr\n";
  die;
}
# here's the query type stuff:
my $qrun = qq{ select startRunTime,runNumber from runDescriptor where startRunTime <= \? order by startRunTime desc limit 1};
my $sthrun = $dbhrun->prepare($qrun);

my $connectstring = join(":","DBI:mysql","Calibrations_ftpc","dbx.star.bnl.gov:3316");
my $dbh = DBI->connect($connectstring,"","");
unless (defined($dbh)) {
  print STDERR "Bad: $DBI::errstr\n";
  die;
}

my $early = "2009-06-16 00:00:00";
my $late = "2009-06-29 23:00:00";


my @results = ();
my $query = "select beginTime,anodeV1East,anodeV2East,anodeV3East,anodeV4East,anodeV5East,anodeV6East,anodeV1West,anodeV2West,anodeV3West,anodeV4West,anodeV5West,anodeV6West from ftpcVoltage where beginTime < \"${late}\" and beginTime > \"${early}\"";
print STDERR $query,"\n";
my $sth2 = $dbh->prepare($query);
$sth2->execute;
my $tbl_ary = $sth2->fetchall_arrayref;
for my $row (@{$tbl_ary}) {
  push @results,[@{$row}];
}





print STDERR scalar(@results), " entries \n";
print STDERR "Done with main sql query.  Starting parse\n";
local $, = " ";
#Now start parsing
my %transitions = ();
my $ntransitions = 0;

{
  my $hvon = 0;
  my $isstarted = 0;
  my $isgood_east =0;
  my $isgood_west =0;

  my $lasttransitiontime =0;
  my @voltsgood = ();
  for my $i (0..11) {
    push @voltsgood,1750;
  }
 LOOPO:for my $ref (@results) {
    my @row = (@{$ref});
    local $\ = "\n";
    my ($year,$ismon,$mday,$hour,$min,$sec) = $row[0] =~ /(\d+)\-(\d+)\-(\d+) (\d+):(\d+):(\d+)/;
    my $goodyear = $year-1900;
    my $goodmon = $ismon-1;
    my $unixtime = timegm($sec,$min,$hour,$mday,$goodmon,$goodyear);

    #  print STDERR scalar(gmtime($unixtime)),$row[1],$row[2];
    #  print $unixtime,$row[0];
    my @voltages = ();
    for my $i (1..12) {
      push @voltages,$row[$i];
    }

    my $thisisgood_east = 1;
    my $thisisgood_west = 1;
    for my $i (0..5) {
      next if ($voltages[$i]>$voltsgood[$i]);
      $thisisgood_east = 0;
    }
    for my $i (6..11) {
      next if ($voltages[$i]>$voltsgood[$i]);
      $thisisgood_west = 0;
    }

    my $transition=0;
    my $delta = 0;
    if (!$isstarted) {
      $transition = 1;
      $isstarted=1;
    }
    if (
	(($isgood_east) && (!$thisisgood_east))
	||
	(($isgood_west) && (!$thisisgood_west))
       )
	{
	  $transition = 1;
	}
    if (
	((!$isgood_east) && ($thisisgood_east))
	||
	((!$isgood_west) && ($thisisgood_west))
       )
      {
	$transition = 1; 
    }

    $lasttransitiontime = $delta;
    $unixtime += $delta;
    if ($transition) {
      # want to be conservative
      # 0 kills 1

      local $\="";
      if (exists($transitions{$unixtime}{east})) {
	print STDERR "Already: $unixtime,",scalar(localtime($unixtime))," with value: ",$transitions{$unixtime}{east},
	  "new value: ",$thisisgood_east; 
	if ($thisisgood_east) {
	  print STDERR "Don't take this one.\n";
	  next LOOPO;
	}
	else {
	  print STDERR "Take new\n";
	}
      }

      if (exists($transitions{$unixtime}{west})) {
	print STDERR "Already: $unixtime,",scalar(localtime($unixtime))," with value: ",$transitions{$unixtime}{west},
	  "new value: ",$thisisgood_west; 
	if ($thisisgood_west) {
	  print STDERR "Don't take this one.\n";
	  next LOOPO;
	}
	else {
	  print STDERR "Take new\n";
	}
      }

      $transitions{$unixtime}{east} = $thisisgood_east;
      $transitions{$unixtime}{west} = $thisisgood_west;
      $isgood_west = $thisisgood_west;
      $isgood_east = $thisisgood_east;

      $ntransitions++;
    }
  }
}
print STDERR "Done with first pass.  Ntransitions = $ntransitions.  Getting rid of duplicate 0's.\n";
open OUT2,">hvtransitions_noparse.txt";

for my $key (sort {$a<=>$b}keys %transitions) {
  local $\="\n";
  print OUT2 $key,$transitions{$key}{east},$transitions{$key}{west},scalar(localtime($key));
}

# check sanity 
{
  my $isstarted = 0;
  my $lasttrans_east = 0;
  my $lasttrans_west = 0;

 LOOP: for my $key (sort {$a<=>$b} keys %transitions) {
    if ($isstarted && $transitions{$key}{east} == $lasttrans_east
       && $transitions{$key}{west} == $lasttrans_west) {
      print STDERR "Screw up on $key,",scalar(localtime($key)),":  two $lasttrans_east and $lasttrans_west in a row\n";
      delete $transitions{$key};
      next LOOP;
    }
    $lasttrans_east = $transitions{$key}{east};
    $lasttrans_west = $transitions{$key}{west};

    $isstarted = 1;
  }
}
for my $i (0..0) {
  print STDERR "Now trying to shift backward the off states by 120 seconds\n";

  my %laston = ();
  {
    my %newtransitions = ();
    my $lastontime_east = 0;
    my $lastontime_west = 0;

    for my $key (sort {$a<=>$b} keys %transitions) {
      # first the off state
      my $tomove = 0;
      if (!($transitions{$key}{east})) {
	if (abs($lastontime_east-$key) < 120) {
	  # delete the on
	  delete($transitions{$lastontime_east});
	  print STDERR "East Off: $key and Last On: $lastontime_east close:  deleting on,",scalar(localtime($lastontime_east)),"\n";
	} 
	$tomove=1;
      }
      else {
	$lastontime_east = $key;
      }
      
      if (!($transitions{$key}{west})) {
	if (abs($lastontime_west-$key) < 120) {
	  # delete the on
	  delete($transitions{$lastontime_west});
	  print STDERR "West Off: $key and Last On: $lastontime_west close:  deleting on,",scalar(localtime($lastontime_west)),"\n";
	} 
	$tomove=1;
      }
      else {
	$lastontime_west = $key;
      }

      if ($tomove) {
	  my $transit_east = $transitions{$key}{east};
	  my $transit_west = $transitions{$key}{west};

	  delete($transitions{$key});
	  my $newtime = $key - 120;
	  $newtransitions{$newtime}{east} = $transit_east;
	  $newtransitions{$newtime}{west} = $transit_west;

	}

    }
    # Fill it back
    for my $key (keys %newtransitions) {
      $transitions{$key}{east} = $newtransitions{$key}{east};
      $transitions{$key}{west} = $newtransitions{$key}{west};

    }
  }
  # check sanity 
  {
    print STDERR "Checking sanity: \n";
    my $isstarted = 0;
    my $lasttrans_east = 0;
    my $lasttrans_west = 0;

  LOOP1: for my $key (sort {$a<=>$b} keys %transitions) {
      if ($isstarted && $transitions{$key}{east} == $lasttrans_east 
	 && $transitions{$key}{west} == $lasttrans_west) {
	print STDERR "Screw up on $key,",scalar(localtime($key)),":  two $lasttrans_east and $lasttrans_west in a row\n";
	delete $transitions{$key};
	next LOOP1;
      }
      $lasttrans_east = $transitions{$key}{east};
      $lasttrans_west = $transitions{$key}{west};

      $isstarted = 1;
    }
  }
  print STDERR "Now trying to shift forward the on states by 120 seconds\n";

  my %nextoff = ();
  {
    my %newtransitions = ();
    my $nextofftime_east = 0;
    my $nextofftime_west = 0;

    for my $key (sort {$b<=>$a} keys %transitions) {
      # first the on state
      my $todelete = 0;

      if ($transitions{$key}{east}) {
	if (abs($nextofftime_east-$key) < 120 &&
	   abs($nextofftime_west-$key) < 120) {
	  # delete this on
	  $todelete = 1;
	}
      }
      else {
	$nextofftime_east = $key;
      }
      if ($transitions{$key}{west}) {
	if (abs($nextofftime_west-$key) < 120 &&
	    abs($nextofftime_east-$key) < 120) {
	  # delete this on
	  $todelete = 1;
	}
      }
      else {
	$nextofftime_west = $key;
      }
      if ($todelete) {
	delete($transitions{$key});
	print STDERR "On: $key and Next Off East: $nextofftime_east Next Off West: $nextofftime_west close:  deleting on,",scalar(localtime($key)),"\n";
	} 
	else {
	  my $transit_east = $transitions{$key}{east};
	  my $transit_west = $transitions{$key}{west};

	  delete($transitions{$key});
	  my $newtime = $key + 120;
	  $newtransitions{$newtime}{east} = $transit_east;
	  $newtransitions{$newtime}{west} = $transit_west;

	}
    }
    # Fill it back
    for my $key (keys %newtransitions) {
      $transitions{$key}{east} = $newtransitions{$key}{east};
      $transitions{$key}{west} = $newtransitions{$key}{west};
    }
  }
  # check sanity 
  {
    my $isstarted = 0;
    my $lasttrans_east = 0;
    my $lasttrans_west = 0;

  LOOP: for my $key (sort {$a<=>$b} keys %transitions) {
      if ($isstarted && $transitions{$key}{east} == $lasttrans_east
	 && $transitions{$key}{west} == $lasttrans_west) {
	print STDERR "Screw up on $key,",scalar(localtime($key)),":  two $lasttrans_east and $lasttrans_west in a row\n";
	delete $transitions{$key};
	next LOOP;
      }
      $lasttrans_east = $transitions{$key}{east};
      $lasttrans_west = $transitions{$key}{west};

      $isstarted = 1;
    }
  }
}

print STDERR "Done with getting HV.  ",scalar(keys %transitions)," transitions.  Parsing run\n";{
  # now go back and parse things for run number
  my %newtransitions = ();
  for my $key (sort {$a<=>$b} keys %transitions) {
    # only change times for transition to good
 #   next unless ($transitions{$key}{east} || $transitions{$key}{west});
    # This finds the start time of the latest run
    $sthrun->execute($key);
    while (my (@row) = $sthrun->fetchrow_array) {
      my $unixtime = $row[0];
      if (abs($key-$unixtime)<240) {
	# Ok, this is a startrun thing
	$unixtime -= 30;
	my $goodfound_east = $transitions{$key}{east};
	my $goodfound_west = $transitions{$key}{west};

	delete $transitions{$key};
	$newtransitions{$unixtime}{east} = $goodfound_east;
	$newtransitions{$unixtime}{west} = $goodfound_west;

	print STDERR "Changed on from from $key,",scalar(localtime($key))," to $unixtime",scalar(localtime($unixtime)),"Run Number",$row[1],"\n";
      }
    }
  }
  # now fill them back in
  for my $key (keys %newtransitions) {
    $transitions{$key}{east} = $newtransitions{$key}{east};
    $transitions{$key}{west} = $newtransitions{$key}{west};

  }
}


# check sanity 
{
  my $isstarted = 0;
  my $lasttrans_east = 0;
  my $lasttrans_west = 0;

 LOOP: for my $key (sort {$a<=>$b} keys %transitions) {
    if ($isstarted && $transitions{$key}{east} == $lasttrans_east
       && $transitions{$key}{west} == $lasttrans_west) {
      print STDERR "Screw up on $key,",scalar(localtime($key)),":  two $lasttrans_east and $lasttrans_west in a row\n";
      delete $transitions{$key};
      next LOOP;
    }
    $lasttrans_east = $transitions{$key}{east};
    $lasttrans_west = $transitions{$key}{west};

    $isstarted = 1;
  }
}

print STDERR "Done with checking sanity.  Outputting\n";
print STDERR scalar(keys %transitions)," keys there\n";
{
  open OUTU,">hvtransitions.txt";
  select OUTU;
  $| = 1;
  for my $key (sort {$a<=>$b}keys %transitions) {
    local $\="\n";
    print OUTU $key,$transitions{$key}{east},$transitions{$key}{west},scalar(localtime($key));
  }
  close OUTU;
  select STDOUT;
}
