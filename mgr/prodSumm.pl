#! /opt/star/bin/perl -w
#
# 
#
# 
#
#
######################################################################
#
# prodSumm.pl
#
# Wensheng Deng 9/99
#
# Update 'operation' table in database and web current-production-summary table
#
# Usage: prodSumm.pl
#

use Net::FTP;
use Class::Struct;
use File::Basename;

require "dbOperaSetup.pl";

struct FileAttri => {
    filename  => '$',
    set       => '$', 
    chain     => '$',
    size      => '$',
    timeS     => '$',
};

my $debugOn = 0;

my $prodPeriod = "prod4";
#my @processes = ("tfs", "tss", "trs");

my $disk0        =  "/disk00000";
my $disk1        =  "/disk00001";
my $topHpssSink  =  "/home/starsink/raw";
my $topHpssReco  =  "/home/starreco/reco";
my $topDisk0Reco =  $disk0 . "/star/starreco";
my $topDisk1Reco =  $disk1 . "/star";

my $sumDir = $topDisk1Reco."/$prodPeriod/sum";

my $prod4_summ_html = "/star/u2e/starreco/prod4/summary/Prod4\.summary\.html";
&beginHtml();

my @Sets = (
	    "auau100/venus412/default/b0_3/year_1s/hadronic_on",
	    "auau100/venus412/default/b3_6/year_1s/hadronic_on",
	    "auau100/venus412/default/b6_9/year_1s/hadronic_on",
	    "auau200/venus412/default/b0_3/year_1b/hadronic_on",
	    "auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on",
	    "auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on"
	    );


my @diskRecoFiles;
my @hpssGeantFiles;
my @hpssRecoFiles;

my %monthHash = (
		 "Jan" => 1,
		 "Feb" => 2, 
		 "Mar" => 3, 
		 "Apr" => 4, 
		 "May" => 5, 
		 "Jun" => 6, 
		 "Jul" => 7, 
		 "Aug" => 8, 
		 "Sep" => 9, 
		 "Oct" => 10, 
		 "Nov" => 11, 
		 "Dec" => 12
		 );

my %yearGeomHash = (
		    "year_1b" => "y1b",
                    "year_1s" => "y1s",
                    "year_2a" => "y2a"
                   );

########## Set variables to find jobs running
my @j_name;
my @j_status;
my $jline = 0;

########## Find reco files in disk
my @diskRecoDirs;
$nDiskFiles = 0;

for( $loopv = 0; $loopv<scalar(@Sets); $loopv++) {
  $diskRecoDirs[$loopv] = "$topDisk1Reco/$Sets[$loopv]/tfs_4";
  print "diskRecoDir: $diskRecoDirs[$loopv]\n" if $debugOn;
}

print "\nFinding reco files in disk\n";
 
foreach $diskDir (@diskRecoDirs) {
  opendir(DIR, $diskDir) or die "can't open $diskDir\n";
  while( defined($filename = readdir(DIR)) ) {
    next if $filename =~ /^\.\.?$/;
    next if $filename =~ /.dst.xdf$/;
    next if $filename =~ /.event.root$/;

    $fullname = $diskDir."\/".$filename;

    my @dirF = split(/\//, $diskDir); 
    my $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[3],$dirF[4],$dirF[5],
		                               $dirF[6],$dirF[7],$dirF[8]);


    my $chainVal = "n\/a";
    if ( $dirF[1] eq "disk00001") {
      my $tpcMode = substr($dirF[9],0,3);
      my $isYear = $yearGeomHash{"$dirF[7]"};
      $chainVal = $tpcMode."_".$isYear;
    }
    
    ($size, $mTime) = (stat($fullname))[7, 9];
    ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
    $mo = sprintf("%2.2d", $mo+1);
    $dy = sprintf("%2.2d", $dy);
  
    if( $yr > 98 ) {
      $fullyear = 1900 + $yr;
    } else {
      $fullyear = 2000 + $yr;
    }

#    $timeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
#		      $fullyear,$mo,$dy,$hr,$min,$sec);

    $timeS = sprintf ("%4.4d%2.2d%2.2d",
		      $fullyear,$mo,$dy);
    
    $fObjAdr = \(FileAttri->new());
    ($$fObjAdr)->filename($filename);
    ($$fObjAdr)->set($set);
    ($$fObjAdr)->chain($chainVal);
    ($$fObjAdr)->size($size);
    ($$fObjAdr)->timeS($timeS);
    $diskRecoFiles[$nDiskFiles] = $fObjAdr;
    $nDiskFiles++;
  }
closedir DIR;
}

print "Total files: $nDiskFiles\n";

########## Find geant files in HPSS

my @hpssGeantDirs;

$nHpssDirs = scalar(@Sets);
$nHpssFiles = 0;

for( $loopv = 0; $loopv<scalar(@Sets); $loopv++) {
  $hpssGeantDirs[$loopv] = "$topHpssSink/$Sets[$loopv]/gstardata";
  print "hpssGeantDir: $hpssGeantDirs[$loopv]\n" if $debugOn;
}

my $ftpHpss = Net::FTP->new("rmds02.rhic.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpHpss->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding geant files in HPSS\n"; 
&walkHpss( $ftpHpss, \@hpssGeantDirs, \@hpssGeantFiles );
print "Total files: ".@hpssGeantFiles."\n";
$ftpHpss->quit();

########## Find reco files in HPSS

my @hpssRecoDirs;

$nHpssDirs = scalar(@Sets);
$nHpssFiles = 0;

for( $loopv = 0; $loopv<scalar(@Sets); $loopv++) {
  $hpssRecoDirs[$loopv] = "$topHpssReco/$Sets[$loopv]/tfs_4";
  print "hpssRecoDir: $hpssRecoDirs[$loopv]\n" if $debugOn;
}

$ftpHpss = Net::FTP->new("rmds02.rhic.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpHpss->login("starreco","MockData") or die "HPSS access failed";

print "\nFinding reco files in HPSS\n"; 
&walkHpss( $ftpHpss, \@hpssRecoDirs, \@hpssRecoFiles );
print "Total files: ".@hpssRecoFiles."\n";
$ftpHpss->quit();

########## declare variables needed to fill the database table and the web table
## for database filling
my $setName = "no";
my $geantFile = "no";
my $geant_size = 0;
my $produced_date = "00000000";
my $pr_chain = "no";

my $job_status = "n\/a";     
my $no_events_complete = 0;

my $disk_dst_date = "00000000";
my $disk_dst_size = 0;
my $disk_hist_date = "00000000";
my $disk_hist_size = 0;
my $hpss_dst_date = "00000000";
my $hpss_dst_size = 0;
my $hpss_hist_date = "00000000";
my $hpss_hist_size = 0;

my $sum_File = "no";
my $jfile_status = "no"; 
my $lb_tag = "no";     
my $lb_ver;

my $mem_size = 0;

my $cpu_event = 0;
my $no_track = 0;
my $no_vertx = 0;
my $jobfile_nm;    


## for web-table filling
my $geantInputSize = 0;
my $geantInputEvts = 0;

my $tfsDstHpssSize = 0;
#my $tssDstHpssSize = 0;
#my $trsDstHpssSize = 0;

my $tfsDstDiskSize = 0;
#my $tssDstDiskSize = 0;
#my $trsDstDiskSize = 0;
 
my $tfsDstEvts = 0;
#my $tssDstEvts = 0;
#my $trsDstEvts = 0;

# total amount 
my $geantInputSizeT = 0;
my $geantInputEvtsT = 0;

my $tfsDstHpssSizeT = 0;
#my $tssDstHpssSizeT = 0;
#my $trsDstHpssSizeT = 0;

my $tfsDstDiskSizeT = 0;
#my $tssDstDiskSizeT = 0;
#my $trsDstDiskSizet = 0;
 
my $tfsDstEvtsT = 0;
#my $tssDstEvtsT = 0;
#my $trsDstEvtsT = 0;

##################################
## find running jobs

  $jline = 0;
  my @CRS_JOB = `ssh rcf.rhic.bnl.gov crs_node_status.pl -c`;
  foreach my $job_line (@CRS_JOB) {
     chop $job_line; 
    my @job_word = split ("_", $job_line);
    my @word_part = split ("%", $job_word[12]);
    $j_name[$jline] = $job_word[10]."_". $job_word[11]."_". $word_part[0];
    $j_status[$jline] = $word_part[1];
     $jline++; 
 } 



## connect to the DB
&StDbOperaConnect();

# need to clear the operation table first here
$sql="delete from $OperationT";
$cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

## 

foreach $eachSet (@Sets) {
  $setName = $eachSet;

  $geantInputSize = 0;
  $geantInputEvts = 0;
  
  $tfsDstHpssSize = 0;
  #$tssDstHpssSize = 0;
  #$trsDstHpssSize = 0;
  
  $tfsDstDiskSize = 0;
  #$tssDstDiskSize = 0;
  #$trsDstDiskSize = 0;
  
  $tfsDstEvts = 0;
  #$tssDstEvts = 0;
  #$trsDstEvts = 0;
  
  foreach $eachGeantFile (@hpssGeantFiles) {

## reinitialize variables
    $geantFile = "no";
    $geant_size = 0;
    $produced_date = "00000000";
    $pr_chain = "no";
    
    $job_status = "n\/a";     
    $no_events_complete = 0;
    
    $disk_dst_date = "00000000";
    $disk_dst_size = 0;
    $disk_hist_date = "00000000";
    $disk_hist_size = 0;
    $hpss_dst_date = "00000000";
    $hpss_dst_size = 0;
    $hpss_hist_date = "00000000";
    $hpss_hist_size = 0;
    $jfile_status = "no";     
    $sum_File = "no";
    
    $lb_tag = "no";     
    
    $mem_size = 0;
    
    $cpu_event = 0;
    $no_track = 0;
    $no_vertx = 0;
    
## end of reinitializing

    $geantFileSet  = ($$eachGeantFile)->set;
    next if ( $geantFileSet ne $setName );

    $geantFile = ($$eachGeantFile)->filename;
    $produced_date  = ($$eachGeantFile)->timeS;
    
    $basename = basename("$geantFile",".fzd");
    $geant_size = (($$eachGeantFile)->size)/1000000;
    $geantInputSize += ($geant_size);
    $basename =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;
    $geantInputEvts += $3;

## check if jobfile is created

 my $run_chain = "tfs";
 my $jSet;
 my @hSet =  split ("/",$eachSet);
    if($hSet[0] eq "auau200") {
    $jSet = $hSet[1]."_".$hSet[2]."_".$hSet[3]."_".$hSet[4] ."_". $hSet[5];
}
  else {
    $jSet = $hSet[0]."_".$hSet[1]."_".$hSet[2]."_".$hSet[3] ."_". $hSet[4] ."_". $hSet[5];
}    
   $jobfile_nm = $jSet . "_" . $basename;
    job_file($run_chain,$jobfile_nm); 
  
## summary info check

    $sumDirTfs = $sumDir . "/tfs";
    opendir(DIR, $sumDirTfs) or die "can't open $sumDirTfs\n";
    while( defined($filename = readdir(DIR)) ) {
      next if $filename =~ /^\.\.?$/;
      next if ( !($filename =~ /.sum$/) );

      if ( $filename =~ /$basename/ ) {
	$sum_File = "yes";
        my $summaryFile = "$sumDirTfs/$basename\.sum";
	&sum_info("$summaryFile",1);
#$chain = "tfs";
	$tfsDstEvts += $no_events_complete;
	last;
      } else {
	next;
      }
    }
    closedir DIR;


## check if job is running

   if($job_status eq "n\/a") {
      my $jj = 0;
     foreach my $job_fname (@j_name) {
    if( $job_fname =~ /$basename/ ) {
      $job_status = $j_status[$jj];
   print $job_fname, "\n";
   print $basename, "\n";
   print $job_status, "\n";
  last;
}
   else {   
    next; 
  } 
      $jj++;
 }
}

## 
#    next if ( $sum_File eq 'no' );
## disk reco file check
  
    foreach $eachDiskRecoFile (@diskRecoFiles) {
      $diskFileSet  = ($$eachDiskRecoFile)->set;
      next if ( $diskFileSet ne $setName );

      $diskRecoFileName = ($$eachDiskRecoFile)->filename;
      next if !($diskRecoFileName =~ /$basename/);

      if ( $diskRecoFileName =~ /.dst.root$/ ) {
	$disk_dst_date = ($$eachDiskRecoFile)->timeS;
	$disk_dst_size = ( ($$eachDiskRecoFile)->size )/1000000;
	$tfsDstDiskSize += $disk_dst_size;
      } elsif ( $diskRecoFileName =~ /.hist.root$/ ) {
	$disk_hist_date = ($$eachDiskRecoFile)->timeS;
	$disk_hist_size = ($$eachDiskRecoFile)->size;
      } else {
	next;
      }

    }

## hpss reco file check
    foreach $eachHpssRecoFile (@hpssRecoFiles) {
      $hpssFileSet  = ($$eachHpssRecoFile)->set;
      next if ( $hpssFileSet ne $setName );

      $hpssRecoFileName = ($$eachHpssRecoFile)->filename;
      next if !($hpssRecoFileName =~ /$basename/);

      if ( $hpssRecoFileName =~ /.dst.root$/ ) {
	$hpss_dst_date = ($$eachHpssRecoFile)->timeS;
	$hpss_dst_size = ( ($$eachHpssRecoFile)->size )/1000000;
	$tfsDstHpssSize += $hpss_dst_size;
      } elsif ( $hpssRecoFileName =~ /.hist.root$/ ) {
	$hpss_hist_date = ($$eachHpssRecoFile)->timeS;
	$hpss_hist_size = ($$eachHpssRecoFile)->size;
      } else {
	next;
      }

    }

## fill operation table in the database
    print "filling operation table\n";
    &fillDbTable();

  }

## accumulate and fill web table
  $geantInputSizeT += $geantInputSize;
  $geantInputEvtsT += $geantInputEvts;
  
  $tfsDstHpssSizeT += $tfsDstHpssSize;

  $tfsDstDiskSizeT += $tfsDstDiskSize;

  $tfsDstEvtsT += $tfsDstEvts;

  $geantInputSize = int($geantInputSize/1000);
  $tfsDstHpssSize = int($tfsDstHpssSize/1000);
  
  $tfsDstDiskSize = int($tfsDstDiskSize/1000);
  
  print "filling web table\n";
  &fillWebTable();

}

&StDbOperaDisconnect();

$geantInputSizeT = int($geantInputSizeT/1000);
$tfsDstHpssSizeT = int($tfsDstHpssSizeT/1000);

$tfsDstDiskSizeT = int($tfsDstDiskSizeT/1000);

&fillWebTableTotal();
&endHtml();

exit;
############
sub fillWebTable {

print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
print HTML "<TD><a href=\"http://duvall.star.bnl.gov/devcgi/dbOperaRetrv.pl?set=$setName\">$setName</TD>\n";
print HTML "<TD>$geantInputSize</TD><TD>$geantInputEvts</TD><TD>$tfsDstHpssSize/0/0</TD><TD>$tfsDstDiskSize/0/0</TD><TD>$tfsDstEvts/0/0</TD></tr>\n";

}

#############
sub fillWebTableTotal {

print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
print HTML "<TD>Total</TD>\n";
print HTML "\n<TD>$geantInputSizeT</TD><TD>$geantInputEvtsT</TD><TD>$tfsDstHpssSizeT/0/0</TD><TD>$tfsDstDiskSizeT/0/0</TD><TD>$tfsDstEvtsT/0/0</TD></tr>\n";
}

############
sub beginHtml {

open (HTML, ">$prod4_summ_html") or die "can't write to $prod4_summ_html";

print HTML "<html>\n";
print HTML "  <head>\n";
print HTML "          <title>Current Production summary</title>\n";
print HTML "  </head>\n";
print HTML "  <body BGCOLOR=\"#ccffff\">\n"; 
print HTML "      <h1>Production summary</h1>\n";
print HTML "<TABLE BORDER=5 CELLSPACING=1 CELLPADDING=2 >\n";
print HTML "<TR>\n";
print HTML "<TR ALIGN=CENTER VALIGN=CENTER>\n";
print HTML "<TD WIDTH=\"20%\" HEIGHT=110><B>Set</B></TD>\n";
print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>GEANT input<br>size (GB)</B></TD>\n";
print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>GEANT input <br>No. of events</B></TD>\n";
print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>tfs/tss/trs dst on HPSS<br>size (GB)</B></TD>\n";
print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>tfs/tss/trs dst on disk<br>size (GB)</B></TD>\n";
print HTML "<TD WIDTH=\"10%\" HEIGHT=110><B>Total DST<br>No. of events<br></B></TD>\n";
print HTML "</TR>\n";

}

############</TABLE>
sub endHtml {

  my $Date = `date`;

print HTML "</TABLE>\n";
print HTML "     <h5>\n";
print HTML "      <address><a href=\"mailto:didenko\@bnl.gov\">Lidia Didenko</a></address>\n";
print HTML "<!-- Created: Tue Set 10  05:29:25 MET 1999 -->\n";
print HTML "<!-- hhmts start -->\n";
print HTML "Last modified: $Date\n";
print HTML "<!-- hhmts end -->\n";
print HTML "  </body>\n";
print HTML "</html>\n";
close HTML;

}

############
sub fillDbTable {

    $sql="insert into $OperationT set ";
    $sql.="SetName='".$setName."',";
    $sql.="GeantFile='$geantFile',";
    $sql.="Geant_size=$geant_size,";
    $sql.="Produced_date='$produced_date',";
    $sql.="Chain='$pr_chain',";
    $sql.="JobStatus='$job_status',";
    $sql.="EventsDone=$no_events_complete,";
    $sql.="Disk_dst_date=$disk_dst_date,";
    $sql.="Disk_dst_size=$disk_dst_size,";
    $sql.="Disk_hist_date=$disk_hist_date,";
    $sql.="Disk_hist_size=$disk_hist_size,";
    $sql.="HPSS_dst_date=$hpss_dst_date,";
    $sql.="HPSS_dst_size=$hpss_dst_size,";
    $sql.="HPSS_hist_date=$hpss_hist_date,";
    $sql.="HPSS_hist_size=$hpss_hist_size,";
    $sql.="Sum_File='$sum_File',";
    $sql.="Jobfile='$jfile_status',"; 
    $sql.="Lib_tag='$lb_tag',";
    $sql.="Mem_size_MB= '$mem_size',";
    $sql.="CPU_per_evt_sec='$cpu_event',";
    $sql.="Ave_No_Tracks=$no_track,";
    $sql.="Ave_No_Vtx=$no_vertx";
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

  }

#############
sub sum_info  {

my ($jb_sum,$useless) = @_;   
my $sum_line ;
#my $job_status;                     # job status 
my @word_sum;
#my $lb_tag;                         # lib tag
#my $lb_ver;                         # lib version
#my $no_track = 0;                   # everage No. tracks 
#my $no_vertx = 0;                   # everage No. verteces  
#my $no_events_complete = 0;         # No. events done 
#my $pr_chain;                       # chain to run
#my $mem_size = 0;                   # max memory size 
#my $cpu_event = 0;                  # cpu per event 
 
my @output = `more $jb_sum`; 
  foreach my $sum_line (@output) {
           chop $sum_line;

# get job status
     if ($sum_line =~ /Segmentation violation/) {
             $job_status = "crash";
         }
    elsif ($sum_line =~ /buss error/) {
            $job_status = "buss_err";
       }  
    elsif ($sum_line =~ /Job status:/) {
       @word_sum = split (":", $sum_line);
         $job_status = $word_sum[1];
       } 

#  get number of events done
   
    if ($sum_line =~ /Number of Events Done/ ) {
      @word_sum = split (":", $sum_line);          
        $no_events_complete = $word_sum[1];
    } 
       
# get library tag
         if ($sum_line =~ /Library version/)  {
          @word_sum = split (":", $sum_line); 
           $lb_ver = $word_sum[1];
        }
       if ($sum_line =~ /from Tag/) {
         @word_sum = split (" ", $sum_line) ;       
           $lb_tag = $word_sum[9];    
       }
# get chain
          if( $sum_line =~ /QAInfo:Requested chain is/ ) {
            @word_sum = split (":", $sum_line); 
              $pr_chain = $word_sum[2];
              $pr_chain =~ s/ /_/g;
           }
# get max memory size during execution
           if ($sum_line =~ /Package   tree:/ ) {
             @word_sum = split (" ", $sum_line);
               $mem_size = $word_sum[5];
           }
# get CPU per event
           if($sum_line =~ /bfc/ ) {
            @word_sum = split (" ", $sum_line);
           if($word_sum[7] =~ /Cpu/) {
              $cpu_event = $word_sum[10];  
          }
        }
         
#  get everage number of tracks in the event

     if($sum_line =~ /QAinfo: number of tracks/) {
      @word_sum = split (" ", $sum_line) ;  
       $no_track = $word_sum[4];
    }   
#  get everage number of vertex in the event

     if($sum_line =~ /QAinfo: number of vertices/) {     
         @word_sum = split (" ", $sum_line) ;
          $no_vertx = $word_sum[4]; 
     }

   }
#  define if not defined

        if (!defined $job_status)  {  
         $job_status = "Not defined"; 
       }

           if(!defined $lb_tag)  {
          $lb_tag = $lb_ver;
        }
 
}

######################
sub job_file($$) {

my $run_ch = $_[0];
my $jfile_nm = $_[1];

my $prod_dir   = "/star/u2e/starreco/prod4/requests/";
my $jobf_dir   = $prod_dir . $run_ch . "/" . "jobfiles";
my $jarch_dir  = $prod_dir . $run_ch . "/" . "archive";
my $jnew_dir   = $prod_dir . $run_ch . "/" . "new_jobs"; 
my $jhold_dir  = $prod_dir . $run_ch . "/" . "jobs_hold";  
 
chdir $jhold_dir;
if (-f $jfile_nm) {$jfile_status = "jobs_hold"};  
chdir $jobf_dir;
if (-f $jfile_nm) {$jfile_status = "jobfiles"};
chdir $jarch_dir;
if (-f $jfile_nm) {$jfile_status = "archive"};
chdir $jnew_dir;
if (-f $jfile_nm) {$jfile_status = "new_jobs"};
 
}
	   
	
######################
sub walkHpss {
  my ( $ftp, $dirs, $files ) = @_;

  for ($ii=0; $ii<$nHpssDirs; $ii++) {
    print "Dir ".$dirs->[$ii]."\n" if $debugOn;
    my @dir = $ftp->dir($dirs->[$ii]);
    for ($jj=0; $jj<@dir; $jj++) {
      my @fields = split(/\s+/, $dir[$jj]);
      my $size   = $fields[4];
      my $month  = $fields[5];
      my $day    = $fields[6];
      my $year   = $fields[7];
      my $name   = $fields[8];

      next if ( $name =~ /^psc/ );
      next if ( $name =~ /^set0337/ );
      next if $name =~ /.dst.xdf$/;
      next if $name =~ /.event.root$/;

      my @dirF = split(/\//, $dirs->[$ii]); 

      my $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
			                         $dirF[7],$dirF[8],$dirF[9]);

      $chainVal = "n\/a";
      if ( $dirF[10] ne 'gstardata' ) {
	$tpcMode = substr($dirF[10],0,3);
	$isYear = $yearGeomHash{"$dirF[8]"};
	$chainVal = $tpcMode."_".$isYear;
      }

      my $monthD = $monthHash{$month};
#      my $sec = 0;
#      my $min = 0;
#      my $hr = 0;
      
      if ( $year =~ m/:/ ) {
	( $hr, $min ) = split(/:/,$year);
	$year = (localtime())[5];
      } else {
	$year = $year - 1900;
      }
      
      if( $year > 98 ) {
	$year = 1900 + $year;
      } else {
	$year = 2000 + $year;
      }
   
   
#      $timeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
#			$year,$monthD,$day,$hr,$min,$sec);
   
      $timeS = sprintf ("%4.4d%2.2d%2.2d",
			$year,$monthD,$day);
      
      $fObjAdr = \(FileAttri->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->set($set);
      ($$fObjAdr)->chain($chainVal);
      ($$fObjAdr)->size($size);
      ($$fObjAdr)->timeS($timeS);
      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
      print "File ".$name."\n" if $debugOn;
    }
  }
}
