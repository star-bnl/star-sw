#!/usr/bin/perl

$anadir="/ldaphome/akio/fps2015";
$datadir="/evp/a";
$year=16;
$start = `date -d "Feb 7 00:00:00" +%s`; $start=~s/\n//g;

$yday=0;
$day=0;
$debug=1;
$submit=0;

if($#ARGV>-1) {
    if($ARGV[0] eq "all") {$yday=-1;} 
    elsif($ARGV[0] eq "0") {$yday=0;} 
    else {
	$yday=$ARGV[0]; $day=substr($yday,2,3); $year=substr($yday,0,2); 
	if($yday<14000 || $yday>20000) {print("yday=$yday is out of range\n"); exit; }
    }    
}
if($#ARGV>0) {
    if($ARGV[1] eq "submit") {$submit=1;}
    if($ARGV[1] eq "run")    {$submit=2;}
}
if($#ARGV>1) {
    if($ARGV[2] eq "debug")   {$debug=2;}
    if($ARGV[2] eq "noprint") {$debug=0;}
}

$out = `ps -elf | grep $anadir/makepage.pl | grep -v grep | grep -v emacs | grep -v csh | wc -l`;
$out=~s/\n//g;
if($debug) {print("ps reports $out $anadir/makepage.pl jobs found\n");}
if ($out > 1) {
    print("There are other $anadir/makepage.pl running. Stop\n");
    `ps -elf | grep $anadir/makepage.pl | grep -v grep | grep -v emacs | grep -v csh`;
    exit;
}else{
    if($debug) {print("No $anadir/makepage.pl running. Go ahead\n");}
}

if($debug) { print("year=$year day=$day submit=$submit\n"); } 

if (-e "$anadir/html")     {} else {`mkdir $anadir/html`;}
$today = ` date +%s`;             $today=~s/\n//g;
$dtoday = ` date +"%Y %b %d %a"`; $dtoday=~s/\n//g;                      
$itoday = ` date +%j`;            $itoday=~s/\n//g;          
if($debug>1) {print "today = $today $dtoday $itoday\n";}

$dstart=`date -d \"UTC 1970-01-01 $start secs\" +"%Y %b %d %a"`;  $dstart=~s/\n//g;
$istart=`date -d \"UTC 1970-01-01 $start secs\" +%j`;  $istart=~s/\n//g;
if($debug>1) {print "start = $start $dstart $istart\n";}

open(OUT,"> $anadir/html/index.php");
print(OUT "<HTML><head><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html\">");
print(OUT "<title>FPS Monitor</title></header><BODY><meta http-equiv=\"Refresh\" content=\"60\">\n");
print(OUT "<?php require('links.php'); links(); ?>\n");

print(OUT "<H1>FPS Monitor</H1>\n");
print(OUT "<table border=1><tr><td>day</td><td>date</td><td># of runs</td><td># of runs with FPS</td></tr>\n");

$diff = 60*60*24+1;
for ($d = $today; $d>=$start; $d-=60*60*24){
    $dd=`date -d \"UTC 1970-01-01 $d secs\" +"%Y %b %d %a"`;
    $ddd=`date -d \"UTC 1970-01-01 $d secs\" +"%b %d"`;
    $id=`date -d \"UTC 1970-01-01 $d secs\" +%j`;
    $dd=~s/\n//g;
    $id=~s/\n//g;
    $id="$year$id";    
    if($debug) {print("id=$id   ");}

    if (-e "$anadir/$id") {} else {`mkdir $anadir/$id`;}
    if (-e "$anadir/$id/condor") {} else {`mkdir $anadir/$id/condor`;}
    if (-e "$anadir/$id/log") {} else {`mkdir $anadir/$id/log`;}
    if ($d == $today) { 
	if($debug) { printf("Getting run list from DB ... "); }
	`cd $anadir; $anadir/runs $id`;
    }
    if (-e "$anadir/$id/run.txt") {} else {
	if($debug){ printf("Getting run list from DB\n");}
	`cd $anadir; $anadir/runs $id`;
    }
    
    $nrun=`wc -l $anadir/$id/run.txt | cut -d " " -f 1`;
    $nrun=~s/\n//g;
    $nfps=`wc -l $anadir/$id/fpsrun.txt | cut -d " " -f 1`;
    $nfps=~s/\n//g;
    if($debug) { printf("nrun=%4d nfpsrun=%4d\n",$nrun,$nfps); }

    if($nfps==0) {next;}    
    print(OUT "<tr><td><a href=\"$id.php\">$id</a></td><td>$dd</td><td>$nrun</td><td>$nfps</td>\n");    

    $diff=$today-$d;
    if($diff<60*60*24) {$isToday=1;}
    else {$isToday=0;}
    if($debug>1) {print("isToday=$isToday\n");}
    if($yday==-1 || ($yday==0 && $isToday==1) || $yday==$id){	
	open(RUNS,"$anadir/$id/run.txt");
	@runlogdb=<RUNS>;
	%runlist=();
	@runs=();
	$nrun=0;
	foreach $_ (@runlogdb){
	    @rundb=split;
	    $run=$rundb[0];
	    $runlist{$run}=1;
	    $runs[$nrun]=$run;
	    $nrun++;
	}
	if($debug>1) {print("Found $nrun runs in DB\n");}
	open(FRUNS,"$anadir/$id/fpsrun.txt");
	@frunlogdb=<FRUNS>;
	$nfrun=0;
	foreach $_ (@frunlogdb){
	    @frundb=split;
	    $frun=$frundb[0];
	    $runlist{$frun}=2;
	    $fruns[$nfrun]=$frun;
	    $nfrun++;
	}
	if($debug>1) {print("Found $nfrun runs with FPS\n");}
	@sortruns=sort(@fruns);
	@revruns=reverse(@sortruns);
	
	$plotdir="$anadir/www/$id/";
	if (-e "$plotdir") {} else {`mkdir -p $plotdir`;}
	
	if($debug) {print("Creating $id.php\n");}
	open(OUT1,"> $anadir/html/$id.php");
	
	print(OUT1 "<HTML><head><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html\">");
	print(OUT1 "<title>FPS Monitor Day$id</title></header><BODY>\n");	    
        print(OUT1 "<script type=\"text/javascript\"><!--\n");
        print(OUT1 "   function openwin(url) {window.open(url, \"\", \"width=800,height=800\")\;} \n");
        print(OUT1 "   // --></script>\n");
	print(OUT1 "<?php require('links.php'); links(); ?>\n");
	
	print(OUT1 "<H1>FPS Monitor Day$id ($dd)</H1>\n");
	print(OUT1 "<a href=\"index.php\">Back to day list</a>\n");
	print(OUT1 "<table border=1>");
	print(OUT1 "<tr><td>Run</td><td>Time</td><td>Length</td><td>Config</td><td>Type</td>\n");
	print(OUT1 "<td>DataSize</td><td>Xing</td><td>ADC2D</td>");
	print(OUT1 "<td>Q1L1</td><td>Q1L2</td><td>Q1L3</td>");
	print(OUT1 "<td>Q2L1</td><td>Q2L2</td><td>Q2L3</td>");
	print(OUT1 "<td>Q3L1</td><td>Q3L2</td><td>Q3L3</td>");
	print(OUT1 "<td>Q4L1</td><td>Q4L2</td><td>Q4L3</td>");
	print(OUT1 "<td>MIP file</td>\n");
	print(OUT1 "<td>Run</td></tr>\n");
	
	$tgt=" onclick=\"openwin(this.href); return false;\"";
	$siz=" Width=100 ";
	$p="png";
	$t="tiny.png";
	foreach $run (@sortruns){
	    
	    $donefile=0;
	    if(-e "$anadir/$id/$run.done") {$donefile++;}
	    
	    $_=`grep $run $anadir/$id/run.txt`;
	    @db = split;
	    $date=@db[1];
	    $time=@db[2];
	    $begin=@db[3];
	    $end=@db[4];
	    $config=@db[5];
	    $type=@db[6];
	    $now=time;
	    $length = $end - $begin;
	    $ago = $now - $end;
	    $time=`date -d \"UTC 1970-01-01 $begin secs\" +"%H:%M:%S"`;  $time=~s/\n//g;
	    if($end==0){$length=-1; $ago=-1;}
	    if($type eq "pedestal") {next;}
	    if($config =~ /Jack/) {next;}
	    if($config =~ /Chris/) {next;}
	    if($config =~ /pp2pp/) {next;}
	    if($config =~ "/pedestal") {$type="pedestal";}
	    if($config =~ "/Pedestal") {$type="pedestal";}
	    if($donefile==0 && $ago>60 && ($type eq "pedestal" || $length>120) ) {		
		$evpcount=0;
		if(-d "$datadir/$run") {
		    $evpcount = `ls $datadir/$run/?* | wc -l`;
		    $evpcount =~ s/\n//g;
		}
		
		if($evpcount>10 && $submit>0) {		    
		    printf("Run$run has no $id/$run.done, $datadir/$run/ has $evpcount events, and run ends $ago (sec) ago, and submit=$submit\n");
		    `touch $anadir/$id/$run.done`;
		    if($submit==2){
			$cmd="$anadir/makeplot $run $type";
			printf("$cmd\n");
			system("$cmd");
		    }elsif($submit==1){
			$condor="$anadir/$id/condor/submit_$run.txt";
			if (-e "$condor") {} else {`\rm $condor`;}
			print("Creating $condor\n");
			open(OUTC, "> $condor\n");
			print(OUTC "Executable   = $anadir/makeplot\n");
			print(OUTC "Universe     = vanilla\n");
			print(OUTC "notification = never\n");
			print(OUTC "getenv       = True\n");
			print(OUTC "Priority    = +1\n");
			print(OUTC "Arguments = $run $type\n");
			print(OUTC "Log    = $anadir/$id/log/$run.condor.log\n");
			print(OUTC "Output = $anadir/$id/log/$run.condor.log\n");
			print(OUTC "Error  = $anadir/$id/log/$run.condor.log\n");
			print(OUTC "Queue\n\n");
			close(OUTC);
			print(OUTC "\n");
			$cmd = "condor_submit ${condor}\n";
			print $cmd;
			system("$cmd");
		    }
		}
	    }
	    
	    print(OUT1 "<tr><td><a href=\"http://online.star.bnl.gov/RunLog/index.php?r=$run\">$run</a></td>");	   
	    print(OUT1 "<td>$time</td><td>$length</td><td>$config</td><td>$type</td>");
	    
	    $f="${run}.size";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.xing";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.2d";    if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q1L1";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q1L2";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q1L3";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q2L1";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q2L2";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q2L3";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q3L1";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q3L2";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q3L3";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q4L1";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q4L2";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.Q4L3";  if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.mip";   if(-e "$plotdir/$f.txt"){print(OUT1 "<td><a href=\"$id\/$f.txt\" $tgt>txt</a></td>");}                          else {print(OUT1 "<td></td>");}
	    print(OUT1 "<td>$run</td></tr>\n");
	}
	print(OUT1 "<tr><td>Run</td><td>Time</td><td>Length</td><td>Config</td><td>Type</td>\n");
	print(OUT1 "<td>DataSize</td><td>Xing</td><td>ADC2D</td>");
	print(OUT1 "<td>Q1L1</td><td>Q1L2</td><td>Q1L3</td>");
	print(OUT1 "<td>Q2L1</td><td>Q2L2</td><td>Q2L3</td>");
	print(OUT1 "<td>Q3L1</td><td>Q3L2</td><td>Q3L3</td>");
	print(OUT1 "<td>Q4L1</td><td>Q4L2</td><td>Q4L3</td>");
	print(OUT1 "<td>MIP file</td>\n");
	print(OUT1 "<td>Run</td></tr>\n");
	print(OUT1 "</table>\n");
	print(OUT1 "<a href=\"index.php\">Back to day list</a>\n");
	print(OUT1 "</BODY></HTML>\n");
    }
}
print(OUT "</table>\n");
print(OUT "<br><b><a href=\"iv/\">IV curves for each IV curve runs</a></b><br>\n");
print(OUT "<img src=\"iv/iv.history.png\">\n");
print(OUT "</BODY></HTML>\n");

`\mv $anadir/html/*.php $anadir/www/`;
`root4star -b -q iv.C`;
