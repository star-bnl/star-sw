#!/usr/bin/env perl
use File::Basename;
use FileHandle;
if ($#ARGV < 0) {
  print "Usage $0 list_of_log_files\n";
  exit 0;
}
# ~/WWW/star/star_machines_speed.txt
#group | nodename         | cpu_speed | hepspec | cpu                                                                                    |
#    1 |.rcas6001-rcas6065 |      2793 |  199.00 | Pentium Xeon X5660 (six-core 12 MB L2) - Dell R410 on SL 5.3 64-bit 
#    2 | rcas6132          |      2000 |   50.00 | Pentium Xeon E5335 Clovertown (quad core, 8MB L2) - Appro 1223x on SL 4.4  
#    3 | rcas6133-rcas6181 |      2670 |  108.00 | Pentium Xeon X5550 Nehalem (quad core 8MB L2) - Dell R710 on SL 5.3 64-bit             |
#    2 | rcas6182          |      2000 |   50.00 | Pentium Xeon E5335 Clovertown (quad core, 8MB L2) - Appro 1223x on SL 4.4              |
#    1 |.rcas6183-rcas6272 |      2793 |  199.00 | Pentium Xeon X5660 (six-core 12 MB L2) - Dell R410 on SL 5.3 64-bit 
#    4 | rcas6273-rcas6334 |      2200 |  306.00 | Intel Xeon E5-2660 Sandy Bridge (eight-core 20 MB L3) - Dell R720 on SL 5.7 64-bit     |
#    5 | rcas6410-rcas6459 |      2800 |  113.00 | Pentium Xeon X5560 Nehalem (quad core 8MB L2) - DelL R710 on SL 5.3 64-bit (fullpower) |
#    1 | rcas6501-rcas6520 |      2793 |  199.00 | Pentium Xeon X5660 (six-core 12 MB L2) - Dell R410 on SL 5.3 64-bit                    |

#    3 | rcrs6001-rcrs6022 |      2670 |  108.00 | Pentium Xeon X5550 Nehalem (quad core 8MB L2) - Dell R710 on SL 5.3 64-bit             |
#    5 | rcrs6100-rcrs6202 |      2800 |  113.00 | Pentium Xeon X5560 Nehalem (quad core 8MB L2) - DelL R710 on SL 5.3 64-bit (fullpower) |
#    4 | rcrs6203-rcrs6250 |      2200 |  306.00 | Intel Xeon E5-2660 Sandy Bridge (eight-core 20 MB L3) - Dell R720 on SL 5.7 64-bit     |
#    1 |.rcrs6251-rcrs6334 |      2793 |  199.00 | Pentium Xeon X5660 (six-core 12 MB L2) - Dell R410 on SL 5.3 64-bit                    |
my $event = 0;
my $tracks = 0;
my $good_tracks = 0;
my $tracksPr = 0;
my $good_tracksPr = 0;
my $tpcHits = 0;
my $tpcHitsUsed = 0;
my $mctracks = 0;
my $ast = 0;
my $cpu = 0;
# ---- CA TPC Tracker ---- 
my $SectRCA = 0;         #Sector reconstruction Time Real =    6157.94 ms, CPU =       6160 ms, parallelization speedup: 1.00033
my $TotalCA = 0;         #Total (sector+merge) reconstuction time Real =    8929.35 ms, CPU =       8920 ms 
my $PrepaCA = 0;         #Preparation time Real =    359.322 ms, CPU =        360 ms
my $AveraCA = 0;         #Avarage sector reconstruction Time Real =    6671.88 ms, CPU =       6655 ms, parallelization speedup: 0.997471
my $sumslCA = 0;         # |  sum slice trackers:    6671.79 ms
my $NeighCA = 0;         # |    NeighboursFinder:    2539.91 ms,   7.0932e+09 cycles
my $StarHCA = 0;         # |     StartHitsFinder:    72.2309 ms
my $TrConCA = 0;         # | TrackletConstructor:    3334.05 ms,  9.31129e+09 cycles
my $TrSelCA = 0;         # |    TrackletSelector:    189.669 ms,    5.295e+08 cycles
my $WriteCA = 0;         # |         WriteOutput:    25.8549 ms
my $mergeCA = 0;         # |               merge:    3494.75 ms
my $TotaACA = 0;         #Total (sector+merge) avarage reconstuction time Real =    10166.6 ms, CPU =      10135 ms
my $count = 0;
my $dummy;
my $node = 0;
print "  event tracks good_tracks tracksPr good_tracksPr tpcHits tpcHitsUsed mctracks ast cpu SectRCA TotalCA PrepaCA AveraCA sumslCA NeighCA StarHCA TrConCA TrSelCA WriteCA mergeCA TotaACA node group cores run GHz\n";
foreach my $file (@ARGV) {
  open(IN,"$file") or die "Can't open $file";
  my $line;
  my $block = 0;
  my $node = 0;
  my $group = 0;
  my $cores = 0;
  my $GHz = 0;
  my $fff = File::Basename::basename($file);
  $fff =~ s/_adc_/_/;
  my @words = split '_', $fff; # print "fff = $fff => words = @words\n";
  my $run = $words[2];
  
  while ($line = <IN>) { #     StAnalysisMaker,  Reading Event:
    if (! $node) {
      if ($line !~ /and node :/) {next;}
      my @words = split ' ', $line;
      $node = $words[12]; print "node = $node";
      $cores = $words[14]; print "\tcores = $cores";
      $GHz   = $words[$#words]; $GHz =~ s/GHz//; print "\t$GHz GHz";
      print "\n";
      if ($cores == 40) {$group = 10;}
      if ($node =~ /^rcrs/) {
	$node =~ s/rcrs//; $node =~ s/\.rcf\.bnl\.gov//;
	if    ($node >= 6001 && node <= 6022) {$group = 3;}
	elsif ($node >= 6100 && node <= 6202) {$group = 5;}
	elsif ($node >= 6203 && node <= 6250) {$group = 4;}
	elsif ($node >= 6251 && node <= 6334) {$group = 1;}
#	if ($node >= 6251 and $node <= 6334) {
#	  print "$node Accepted\n";
#	} else {
#	  print "$node Rejected\n";
#	  $node = 0;
#	  last;
#	}
      } elsif ( $node =~ /^rcas/) {
	$node =~ s/rcas//; $node =~ s/\.rcf\.bnl\.gov//;
	if    ($node >= 6001 && node <= 6065) {$group = 1;}
	elsif ($node == 6132                ) {$group = 2;}
	elsif ($node >= 6133 && node <= 6181) {$group = 3;}
	elsif ($node == 6182                ) {$group = 2;}
	elsif ($node >= 6183 && node <= 6272) {$group = 1;}
	elsif ($node >= 6273 && node <= 6334) {$group = 4;}
	elsif ($node >= 6410 && node <= 6459) {$group = 5;}
	elsif ($node >= 6501 && node <= 6520) {$group = 1;}
	$node = -$node;
#	if ($node >= 6001 and $node <= 6065 ||
#	    $node >= 6183 and $node <= 6272) {
#	  print "$node Accepted\n";
#	} else {
#	  print "$node Rejected\n";
#	  $node = 0;
#	  last;
#	}
	
      } else {
	die "NUnrecognized node = $node\n";
      }
      $next;
    }
    if (! $block and 
	$line !~ /StAnalysisMaker,  Reading Event:/ and
	$line !~ /---- CA TPC Tracker ----/
       ) {next;}
    else {
      if (! $block) {$block = 1;}
      # print $line;
      chop($line);
      $line =~ s/\\[0m//g;
      $line =~ s/\\[31m//g;
      $line =~ s/\\[32m//g;
      $line =~ s/\\[33m//g;
      $line =~ s/\\[34m//g;
      $line =~ s/\\[35m//g;
      #print "$line\n";
      my @w = split ' ', $line;
      my @col = split ':',$line;
      if ($line =~ /StAnalysisMaker,  Reading Event:/) {
	$event = $w[6];  #print "======>Event no. = $event\n";
      } elsif ($line =~ /QA :INFO  - # track nodes:/) {
	$tracks = $col[2]; 
	$good_tracks = $col[4]; 
	#print "======>tracks = $tracks good_tracks = $good_tracks\n";
      } elsif ($line =~ 'U\/T\/G') {
        my $temp = $col[5]; #print "temp = $temp\n";
	$temp =~ s/TPC.*//; #print "temp = $temp\n";
	($dum,$tracksPr,$good_tracksPr) = split(',',$temp);
	#print "======>tracksPr = $tracksPr good_tracksPr = $good_tracksPr\n";
      } elsif ($line =~ 'QA \:INFO  - \# TPC hits\:') {
	$tpcHits = $col[2]; $tpcHits =~ s/://;
	$tpcHitsUsed = $col[6]; 
	#print "======>tpcHits = $tpcHits tpcHitsUsed = $tpcHitsUsed\n";
      } elsif ($line =~ /appended mc tracks:/) {
	$mctracks = $w[3];
	#print "======>mctracks = $mctracks\n";
      } elsif ($line =~ 'QA :INFO  - QAInfo: Done with Event \[no') {
	$ast = $w[16];
	$cpu = $w[21];
	#print "======>ast = $ast cpu = $cpu\n";
	$block = 0;
	#print "end of block =====================================================================\n";
	print "Ev. $event $tracks $good_tracks $tracksPr $good_tracksPr $tpcHits $tpcHitsUsed $mctracks $ast $cpu $SectRCA $TotalCA $PrepaCA $AveraCA $sumslCA $NeighCA $StarHCA $TrConCA $TrSelCA $WriteCA $mergeCA $TotaACA $node $group $cores $run $GHz\n";
      } elsif ($line =~ /Sector reconstruction Time Real =/) {
	$SectRCA = $w[5];
	#print "SectRCA = $SectRCA\n";
      } elsif ($line =~ /Preparation time Real/) {
	$PrepaCA = $w[4];
	#print "PrepaCA = $PrepaCA\n";
      } elsif ($line =~ /Total .* avarage reconstuction time Real/) {
	$TotaACA = $w[7];
	#print "TotaACA = $TotaACA\n";
	$block = 0;
      } elsif ($line =~ /Total .* reconstuction time Real/) {
	$TotalCA = $w[7];
	#print "TotalCA = $TotalCA\n";
      } elsif ($line =~ /Avarage sector reconstruction Time Real/) {
	$AveraCA = $w[6];
	#print "AveraCA = $AveraCA\n";
      } elsif ($line =~ /sum slice trackers/) {
	$sumslCA = $w[4];
	#print "sumslCA = $sumslCA\n";
      } elsif ($line =~ /NeighboursFinder:/) {
	$NeighCA = $w[2];
	#print "NeighCA = $NeighCA\n";
      } elsif ($line =~ /StartHitsFinder:/) {
	$StarHCA = $w[2];
	#print "StarHCA = $StarHCA\n";
      } elsif ($line =~ /TrackletConstructor:/) {
	$TrConCA = $w[2];
	#print "TrConCA = $TrConCA\n";
      } elsif ($line =~ /TrackletSelector:/) {
	$TrSelCA = $w[2];
	#print "TrSelCA = $TrSelCA\n";
      } elsif ($line =~ /WriteOutput:/) {
	$WriteCA = $w[2];
	#print "WriteCA = $WriteCA\n";
      } elsif ($line =~ /merge:/) {
	$mergeCA = $w[2];
	#print "mergeCA = $mergeCA\n";
      }
    }
    $count++;
#    if ($count > 160) {last;}
  }
  close(IN);
}

