#!/opt/star/bin/perl -w
#
# L.Didenko
# 
# Script to find and replace lost production files on HPSS. If file not found mark it
# as unavailable in FileCatalog.
# Script requires list of lost files as argument, placed under /star/u/starreco.
# Create FCrmRootfiles.csh, when run it lost and not found files will be mark as unavailable 
# in FileCatalog. 
# Script should be run on rcas6003 with starreco account to make possible to login to all rcas nodes
#
###########################################################################################################

use lib "/afs/rhic/star/packages/scripts";
use FileCatalog;

my $SITE         = "LBL";
my $status       = (0==1);

my $fileC = new FileCatalog();

$fileC->connect_as($SITE."::User","FC_user") || die "Connection failed for FC_user\n";

 my $listname = $ARGV[0];

 my @fileList = ();

  open (RUNLIST, $listname ) or die "cannot open $listname: $!\n";

  @fileList = <RUNLIST>;

  close(RUNLIST);

  my @prt = ();
  my @ppr = ();
  my $pfile;
  my $mpath;
  my $pathname;
  my $dpath;
  my $dnode;
  my @data;
  my $nn = 0;
  my $tophpss = "/home/starreco/reco";
  my $toppath = "/star/data07/gridwork/reco";
  my $pathpass;
  my $rcfname;
  my $rcfpath;
  my $fullname;
  my $hpsspath;
  my $tmpath = "/star/data07/gridwork/reco";

 my $script = "/home/didenko/get_files.pdsf.csh";
 my $copyscript = "/home/didenko/copy_hpss.csh";
 my $fCscript = "/home/didenko/restore_files.csh";  

   open (FILE, ">$script");
   print FILE "#! /usr/local/bin/tcsh -f", "\n";

   open (CFILE, ">$copyscript");
   print CFILE "#! /usr/local/bin/tcsh -f", "\n";
   print CFILE "pftp -v hpss.rcf.bnl.gov 4021 <<EOF | tee -a copy_grid.log", "\n";
   print CFILE "quote site setcos 11", "\n";
   print CFILE "bin", "\n";

    open (SFILE, ">$fCscript"); 
    print CFILE "#! /usr/local/bin/tcsh -f", "\n";  


  foreach my $line (@fileList)  {
      chop $line;
  @prt = split("home", $line);
  $pathname = "/home".$prt[1];
   
  if($pathname =~ /st_fast/) {
  @ppr = split("st_fast", $pathname);
  $mpath = substr($ppr[0],0,-1);

  $pfile = "st_fast".$ppr[1]; 

  }elsif($pathname =~ /st_/ ) {

  @ppr = split("st_", $pathname);
  
  $mpath = substr($ppr[0],0,-1);

  $pfile = "st_".$ppr[1]; 

  } 
  $pathpass = $mpath;
  $pathpass =~ s/$tophpss//g;
#      print $pathpass, "\n";
  @prt = ();
  @data = ();  
      if($pfile =~ /.MuDst.root/) {
  $fileC->set_context("path~$pathpass","filename=$pfile","available=1","storage=hpss");
  @data = $fileC->run_query("path");

  if(scalar(@data) >= 1) {  
  foreach my $dline (@data)  {
#      print $dline, "\n";

  $dpath = $dline; 
   print "File found    ",  $dpath,"/",$pfile,"\n";

  @prt = split("reco",$dline);

  $rcfpath = $toppath.$prt[1]; 
#  print "Path at RCF   ",$rcfpath,"\n";
  $fullname = $dpath."/".$pfile;
  $rcfname = $rcfpath."/".$pfile;
  $hpsspath = $rcfpath;
  
 $hpsspath =~ s|$toppath|$tophpss|g; 

 print FILE "mkdir -p  $rcfpath","\n";
 print FILE "globus-url-copy -vb gsiftp://garchive.nersc.gov",$fullname, " file:",$rcfname, "\n";

  print SFILE "fC_cleanup.pl -doit -mark on -cond path=$mpath,filename=$pfile,storage=HPSS,available=0","\n";

        if($tmpath eq $rcfpath) {

    print CFILE "pput ", $pfile,"\n";
  }else{
    print CFILE "lcd ", $rcfpath,"\n";
    print CFILE "cd ", $hpsspath,"\n";
    print CFILE "pput ", $pfile,"\n";
  
  $tmpath = $rcfpath;

   }
  }
 }

  $fileC->clear_context();



   }
  }

 print CFILE "__EOF__","\n";
 print CFILE "exit","\n";

 close(FILE);
 close(CFILE); 
 close(SFILE);
# `chmod +x $script`;


exit;
