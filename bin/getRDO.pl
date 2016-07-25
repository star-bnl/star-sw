#!/usr/bin/env perl 
$ENV{http_proxy} = "";
$ENV{ftp_proxy} = "";
#use HTTP::Date;
#my @times = ('2005-01-19 18:27:31','2005-04-19 18:27:31');
#foreach my $begin (@times) {
#  my ($beginDate,$beginTime) = split ' ', $begin; print "$begin => $beginDate,$beginTime\n";
#  my ($month,$day,$year) = split '/', $beginDate;
#  $beginTime =~ s/\..*$//;
#  my ($hour,$mins,$secs) = split ':', $beginTime;
#  $beginDate = $year . "-" . $month . "-" . $day;
#  my $utime = HTTP::Date::str2time($begin);
#  $utime += 2*3600;
#  if (!($month < 4 or $month == 4 and  $day < 3 or $month == 4 and $day == 3 and $hour < 2)) {$utime -= 3600;}
#  my $newbegin =  HTTP::Date::time2str($utime);
#  print "$begin => $utime => $newbegin\n";
#}
#die;
# http://svtmonitor.starp.bnl.gov/archive/cgi/CGIExport.cgi?DIRECTORY=%2Fdata%2Farchivedata%2FRunIII_IV_V%2FSVTDATA&PATTERN=&NAMES=
#SVT%3AP1_T1_RDOW1%0D%0ASVT%3AP1_T2_RDOW1%0D%0ASVT%3AP1_T3_RDOW1%0D%0ASVT%3AHV_LVFault_P1_RDOW1%0D%0ASVT%3AHV_RD_MC_P1_RDOW1%0D%0ASVT%3AHV_RD_MV_P1_RDOW1%0D%0A
#&STARTMONTH=01&STARTDAY=13&STARTYEAR=2005&STARTHOUR=00&STARTMINUTE=00&STARTSECOND=00&ENDMONTH=01&ENDDAY=13&ENDYEAR=2005&ENDHOUR=23&ENDMINUTE=59&ENDSECOND=59&YLOW=0&YHIGH=1&COMMAND=GET&FORMAT=SPREADSHEET&INTERPOL=0

# source ~deph/online/Conditions/svt/svtRDOsSender_i.cc
#
#my $url = "http://svtmonitor.starp.bnl.gov/archive/cgi/CGIExport.cgi?DIRECTORY=%2Fdata%2Farchivedata%2FSVTDATA&PATTERN=&NAMES=DCTest_E5%0D%0A&STARTMONTH=01&STARTDAY=01&STARTYEAR=2007&STARTHOUR=00&STARTMINUTE=00&STARTSECOND=00&ENDMONTH=05&ENDDAY=01&ENDYEAR=2007&ENDHOUR=23&ENDMINUTE=59&ENDSECOND=59&YLOW=0&YHIGH=1&COMMAND=GET&FORMAT=PLOT&INTERPOL=0"
#my $url = "http://svtmonitor.starp.bnl.gov/archive/cgi/CGIExport.cgi?DIRECTORY=%2Fdata%2Farchivedata%2FRunIII_IV_V%2FSVTDATA&PATTERN=&NAMES=SVT";
my $url = "http://svtmonitor.starp.bnl.gov/archive/cgi/CGIExport.cgi?DIRECTORY=%2Fdata%2Farchivedata%2FSVTDATA&PATTERN=&NAMES=SVT";
#my $time1='&STARTMONTH=02&STARTDAY=20&STARTYEAR=2005&STARTHOUR=00&STARTMINUTE=00&STARTSECOND=00'; 
#my $time2='&ENDMONTH=02&ENDDAY=21&ENDYEAR=2005&ENDHOUR=23&ENDMINUTE=59&ENDSECOND=59&';
#my $time1='&STARTMONTH=01&STARTDAY=20&STARTYEAR=2005&STARTHOUR=00&STARTMINUTE=00&STARTSECOND=00';
#my $time2='&ENDMONTH=07&ENDDAY=21&ENDYEAR=2005&ENDHOUR=23&ENDMINUTE=59&ENDSECOND=59&';
my $time1='&STARTMONTH=01&STARTDAY=01&STARTYEAR=2007&STARTHOUR=00&STARTMINUTE=00&STARTSECOND=00';
my $time2='&ENDMONTH=07&ENDDAY=21&ENDYEAR=2007&ENDHOUR=23&ENDMINUTE=59&ENDSECOND=59&';
my $dummy = "YLOW=0&YHIGH=1&COMMAND=GET&FORMAT=SPREADSHEET&INTERPOL=0";
my  @port=(
	   2,2,2,2,2,2,2,2,1,2,3,1,
	   2,3,1,2,3,1,2,3,1,3,1,3,
	   1,3,1,3,1,3,1,3,1,3,1,3,
	   2,2,2,2,2,2,2,2,1,2,3,1,
	   2,3,1,2,3,1,2,3,3,1,3,1,
	   3,1,3,1,3,1,3,1,3,1,3,1);
my  @RDO=("W1","W2","W4","W5","W7","W8","W10","W11","W3","W3","W3","W6",
	  "W6","W6","W9","W9","W9","W12","W12","W12","W1","W1","W2","W2",
	  "W4","W4","W5","W5","W7","W7","W8","W8","W10","W10","W11","W11",
	  "E1","E2","E4","E5","E7","E8","E10","E11","E3","E3","E3","E6",
	  "E6","E6","E9","E9","E9","E12","E12","E12","E1","E1","E2","E2",
	  "E4","E4","E5","E5","E7","E7","E8","E8","E10","E10","E11","E11");
if ($#port != 71 || $#RDO != 71) {die "Problem with array size port $#port != 71 or RDO $#RDO != 71";}
my $url = 'http://svtmonitor.starp.bnl.gov/archive/cgi/CGIExport.cgi?DIRECTORY=%2Fdata%2Farchivedata%2FRunIII_IV_V%2FSVTDATA&PATTERN=&NAMES=';
my ($beginDate,$beginTime,$flavor,$deactive,$barNum,$ladNum,$rdo,$northTemp,$southTemp,$hvBoardTemp,$hvVolt,$hvCurr,$lvFault) = 
  (0,0,"SLC",0,0,0,"",0,0,0,0,0,0);
#               72
for(my $i=0; $i < 72; $i++){
  my $file = "SLC" . $i . ".text";
  open(OUT,">$file") or die "Can't open $file";
#  print "beginTime       flavor  deactive        barNum  ladNum  rdo     northTemp       southTemp       hvBoardTemp     hvVolt  hvCurr  lvFault\n";
#  print OUT "beginTime       flavor  deactive        barNum  ladNum  rdo     northTemp       southTemp       hvBoardTemp     hvVolt  hvCurr  lvFault\n";
#  print "i = $i\n";
  my $names = 
    "SVT%3AP" . $port[$i] . "_T1_RDO" . $RDO[$i] . "%0D%0A" .       #          northTemp  ;      /*sensors at the end of each half-ladder*/ 	  
    "SVT%3AP" . $port[$i] . "_T2_RDO" . $RDO[$i] . "%0D%0A" . 	    #          southTemp  ;      /*and should track the water temp*/		  
    "SVT%3AP" . $port[$i] . "_T3_RDO" . $RDO[$i] . "%0D%0A" .       #          hvBoardTemp;      /*adjacent to the edge detector */		  
    "SVT%3AHV_RD_MV_P" . $port[$i] . "_RDO" . $RDO[$i] . "%0D%0A" . #          hvVolt     ;      /* hvVolt is approximately 1500 (or -1500) */  
    "SVT%3AHV_RD_MC_P" . $port[$i] . "_RDO" . $RDO[$i] . "%0D%0A" . #          hvCurr     ;      /*If hvCurr is far from 4500 uA (or -4500) */  
    "SVT%3AHV_LVFault_P" . $port[$i] . "_RDO" . $RDO[$i] . "%0D%0A";#          lvFault    ;      /*if lvFault=true the hybrids are not working*/
#  print "$names\n";
  $rdo = $RDO[$i];
  if($i<8){             #barrel 1 West
    $barNum=1;
    $ladNum=$i+1;       #sets ladder num = 1-8  
  } elsif ($i<20) {     #barrel 2 West
    $barNum=2;     
    $ladNum=$i-7;       #sets ladder num = 1-12  
  } elsif ($i<36) {     #barrel 3 West
    $barNum=3;
    $ladNum=$i-19;      #sets ladder num = 1-16  
  } elsif ($i<44) {     #barrel 1 West
    $barNum=1;
    $ladNum=$i-35;      #sets ladder num = 1-8  
  } elsif ($i<56) {     #barrel 2 West
    $barNum=2;
    $ladNum=$i-43;      #sets ladder num = 1-12  
  } else  {             #barrel 3 West 
    $barNum=3;
    $ladNum=$i-55;      #sets ladder num = 1-16    
  }
  my $cmd = "lynx -dump " . "'" .$url . $names . $time1 . $time2 . $dummy . "'";
  print "$cmd\n";
  my @lines = `$cmd`;
  foreach my $line (@lines) {
    next if ! $line;
#    print $line;
#    print " \t$northTemp ,$southTemp ,$hvBoardTemp ,$hvVolt ,$hvCurr ,$lvFault\n";
#    $line =~ s|#N/A|0\.00|g;
    ($beginDate,$beginTime,$lnorthTemp,$lsouthTemp,$lhvBoardTemp,$lhvVolt,$lhvCurr,$llvFault) = split ' ', $line;
#    print "l\t$lnorthTemp ,$lsouthTemp ,$lhvBoardTemp ,$lhvVolt ,$lhvCurr ,$llvFault\n";
#    next if !$beginDate or !$beginTime;
    my ($month,$day,$year) = split '/', $beginDate;
    next if $year ne '2005';
    my ($pnorthTemp,$psouthTemp,$phvBoardTemp,$phvVolt,$phvCurr,$plvFault) = ($northTemp,$southTemp,$hvBoardTemp,$hvVolt,$hvCurr,$lvFault);
    $pnorthTemp = $lnorthTemp if $lnorthTemp ne '#N/A';
    $psouthTemp = $lsouthTemp if $lsouthTemp ne '#N/A';
    $phvBoardTemp = $lhvBoardTemp if $lhvBoardTemp ne '#N/A';
    $phvVolt = $lhvVolt if $lhvVolt ne '#N/A';
    $phvCurr = $lhvCurr if $lhvCurr ne '#N/A';
    $plvFault = $llvFault if $llvFault ne '#N/A';
#    print "p\t$pnorthTemp ,$psouthTemp ,$phvBoardTemp ,$phvVolt ,$phvCurr ,$plvFault\n";
    next if ($pnorthTemp   eq $northTemp   and
	     $psouthTemp   eq $southTemp   and
	     $phvBoardTemp eq $hvBoardTemp and
	     $phvVolt      eq $hvVolt      and
	     $phvCurr      eq $hvCurr      and
	     $plvFault     eq $lvFault);      
    $northTemp   = $pnorthTemp;
    $southTemp   = $psouthTemp;
    $hvBoardTemp = $phvBoardTemp;
    $hvVolt      = $phvVolt;
    $hvCurr      = $phvCurr;
    $lvFault     = $plvFault;
    $beginTime =~ s/\..*$//;
    my ($hour,$mins,$secs) = split ':', $beginTime;
    $beginDate = $year . "-" . $month . "-" . $day;
#    my $utime = HTTP::Date::str2time($beginTime);
#    if ($month < 4 or $month == 4 and  $day < 3 or $month == 4 and $day == 3 and $hour < 2) {$utime -= 3600;}
#    next  if ! (int $northTemp or int $southTemp or int $hvBoardTemp or int $hvVolt or int $hvCurr or int $lvFault);
#    print "$beginDate $beginTime $flavor $deactive $barNum $ladNum $rdo";
#    printf("%10.4f %10.4f %10.4f %10.4f %10.4f %2i\n", ,$northTemp ,$southTemp ,$hvBoardTemp ,$hvVolt ,$hvCurr ,$lvFault);
    print OUT "$beginDate $beginTime $flavor $deactive $barNum $ladNum $rdo";
    printf OUT "%10.4f %10.4f %10.4f %10.4f %10.4f %2i\n",($northTemp ,$southTemp ,$hvBoardTemp ,$hvVolt ,$hvCurr ,$lvFault);
  }
  close(OUT);
}
