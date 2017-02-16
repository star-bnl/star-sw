#!/usr/bin/perl

$anadir="/ldaphome/akio/rcc2016";
$datadir="/trg/trgdata";
#$datadir=".";
$year=17;
$start = `date -d "2016/03/16 00:00:00" +%s`; $start=~s/\n//g;

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
    if($ARGV[1] eq "noqa")   {$submit=3;}
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
#$today = ` date +%s --date='2016/12/31'`;     $today=~s/\n//g;
#$dtoday = ` date +"%Y %b %d %a" --date='2016/12/31'`; $dtoday=~s/\n//g;
#$itoday = ` date +%j --date='2016/12/31'`;            $itoday=~s/\n//g;
if($debug>1) {print "today = $today $dtoday $itoday\n";}

$dstart=`date -d \"UTC 1970-01-01 $start secs\" +"%Y %b %d %a"`;  $dstart=~s/\n//g;
$istart=`date -d \"UTC 1970-01-01 $start secs\" +%j`;  $istart=~s/\n//g;
if($debug>1) {print "start = $start $dstart $istart\n";}

open(OUT,"> $anadir/html/index.php");
print(OUT "<HTML><head><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html\">");
print(OUT "<title>RCC Counter Monitor</title></header><BODY><meta http-equiv=\"Refresh\" content=\"60\">\n");
print(OUT "<?php require('links.php'); links(); ?>\n");

print(OUT "<H1>RCC Counter Monitor</H1>\n");
print(OUT "<table border=1><tr><td>day</td><td>date</td><td># of runs</td><td>History</td><td>Zoom</td></tr>\n");

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
    if($debug) { printf("nrun=%4d\n",$nrun); }

    if($nrun==0) {next;}    
#    print(OUT "<tr><td><a href=\"$id.php\">$id</a></td><td>$dd</td><td>$nrun</td>\n");    

    $diff=$today-$d;
    if($diff<60*60*24) {$isToday=1;}
    else {$isToday=0;}
    if($debug>1) {print("isToday=$isToday\n");}
    if($yday==-1 || ($yday==0 && $isToday==1) || $yday==$id){	
	open(RUNS,"$anadir/$id/run.txt");
	@runlogdb=<RUNS>;
	%runlist=();
	@runxs=();
	$nrun=0;
	foreach $_ (@runlogdb){
	    @rundb=split;
	    $run=$rundb[0];
	    $runlist{$run}=1;
	    $runs[$nrun]=$run;
	    $nrun++;
	}
	if($debug>1) {print("Found $nrun runs in DB\n");}
	@sortruns=sort(@runs);
	@revruns=reverse(@sortruns);
	
	$plotdir="$anadir/www/$id/";
	if (-e "$plotdir") {} else {`mkdir -p $plotdir`;}
	
	if($debug) {print("Creating $id.php\n");}
	open(OUT1,"> $anadir/html/$id.php");
	
	print(OUT1 "<HTML><head><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html\">");
	print(OUT1 "<title>RCC Counter Monitor Day$id</title></header><BODY>\n");	    
	print(OUT1 "<?php require('links.php'); links(); ?>\n");
	
	print(OUT1 "<H1>RCC Counter Monitor Day$id ($dd)</H1>\n");
	print(OUT1 "<a href=\"index.php\">Back to day list</a>\n");
	print(OUT1 "<table border=1><tr><td>Run</td><td>Time</td><td>Length</td><td>Config</td><td>Type</td>\n");
	print(OUT1 "<td>RCC-TCU</td><td>RCC-TCU zoom</td><td>Time vs RCC-TCU</td></tr>\n");
	
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
#	    if($type eq "pedestal") {next;}
	    if($config =~ /tcd_only/) {next;}
#	    if($config =~ /Ped/) {next;}
#	    if($config =~ /btow/) {next;}
#	    if($config =~ /chris/) {next;}
#	    if($length < 120) {next;}

	    $filecount=0;
	    if(-e "$datadir/run$run.1.dat") {$filecount++;}
	    if(-e "$datadir/run$run.2.dat") {$filecount++;}
#	    if($filecount<2) {next;}
	    if($filecount<1) {next;}
	    
	    if(($donefile==0 || $submit==3) && $submit>0) {		    
		printf("Run$run has no $id/$run.done, $datadir/ has files, and run ends $ago (sec) ago, and submit=$submit\n");
		`touch $anadir/$id/$run.done`;
		if($submit==3){
		    $cmd="$anadir/makeplot $run noqa";
		    printf("$cmd\n");
		    system("$cmd");
		}elsif($submit==2){
		    $cmd="$anadir/makeplot $run";
		    printf("$cmd\n");
		    system("$cmd");
		}elsif($submit==1){
		    $condor="$anadir/$id/condor/submit_$run.txt";
		    if (-e "$condor") {} else {`\rm $condor`;}
		    print("Creating $condor\n");
		    open(OUTC, "> $condor\n");
		    print(OUTC "Executable   = makeplot\n");
		    print(OUTC "Universe     = vanilla\n");
		    print(OUTC "notification = never\n");
		    print(OUTC "getenv       = True\n");
		    print(OUTC "Priority    = +1\n");
		    print(OUTC "Arguments = $run\n");
		    print(OUTC "Log    = $anadir/$id/log/$run.log\n");
		    print(OUTC "Output = $anadir/$id/log/$run.log\n");
		    print(OUTC "Error  = $anadir/$id/log/$run.log\n");
		    print(OUTC "Queue\n\n");
		    close(OUTC);
		    print(OUTC "\n");
		    $cmd = "condor_submit ${condor}\n";
		    print $cmd;
		    #system("condor_submit ${condor}\n");
		}
	    }
	    
	    print(OUT1 "<tr><td><a href=\"http://online.star.bnl.gov/RunLog/index.php?r=$run\">$run</a></td>");	   
	    print(OUT1 "<td>$time</td><td>$length</td><td>$config</td><td>$type</td>");	    
	    $f="${run}.diff";      if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.diff.zoom"; if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    $f="${run}.tdiff";     if(-e "$plotdir/$f.$p") {print(OUT1 "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT1 "<td></td>");}
	    print(OUT1 "<tr>\n");
	}
	print(OUT1 "</table>\n");
	print(OUT1 "</BODY></HTML>\n");

	$cmd="cd $anadir; $anadir/makehistory $id 0 $debug"; if($debug) {print("$cmd\n");}
        $out=system("$cmd"); if($debug) {print("$out\n");}
        $out=system("\mv $anadir/$id/history.$id.png $anadir/www/$id/"); if($debug) {print("$out\n");}
        $out=system("convert $anadir/www/$id/history.$id.png -equalize -geometry 100x100 $anadir/www/$id/history.$id.tiny.png");
        if($debug) {print("$out\n");}
	$cmd="cd $anadir; $anadir/makehistory $id 1 $debug"; if($debug) {print("$cmd\n");}
        $out=system("$cmd"); if($debug) {print("$out\n");}
        $out=system("\mv $anadir/$id/historyz.$id.png $anadir/www/$id/"); if($debug) {print("$out\n");}
        $out=system("convert $anadir/www/$id/historyz.$id.png -equalize -geometry 100x100 $anadir/www/$id/historyz.$id.tiny.png");
    }
    $plotdir="$anadir/www/$id/";
    print(OUT "<tr><td><a href=\"$id.php\">$id</a></td><td>$dd</td><td>$nrun</td>");
    $f="history.${id}";  if(-e "$plotdir/$f.$p") {print(OUT "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT "<td></td>");}
    $f="historyz.${id}"; if(-e "$plotdir/$f.$p") {print(OUT "<td><a href=\"$id\/$f.$p\" $tgt><img src=\"$id\/$f.$t\" $siz></a></td>");} else {print(OUT "<td></td>");}
    print(OUT "\n");
}
print(OUT "</table>\n");

if($yday<1){
    $cmd="cd $anadir; $anadir/makehistory ${year}000 0 $debug";
    if($debug) {print("$cmd\n");}
    $out=system("$cmd");  if($debug) {print("$out\n");}
    $out=system("\mv $anadir/history.png $anadir/www/"); if($debug) {print("$out\n");}
    $cmd="cd $anadir; $anadir/makehistory ${year}000 1 $debug";
    if($debug) {print("$cmd\n");}
    $out=system("$cmd");  if($debug) {print("$out\n");}
    $out=system("\mv $anadir/historyz.png $anadir/www/"); if($debug) {print("$out\n");}
}
print(OUT "<li>History(zoom)<br><img src=\"historyz.png\">\n");
print(OUT "<li>History(full scale)<br><img src=\"history.png\">\n");

print(OUT "</BODY></HTML>\n");

`\mv $anadir/html/*.php $anadir/www/`;
