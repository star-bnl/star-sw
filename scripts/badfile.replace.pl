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

my $SITE         = "BNL";
my $status       = (0==1);

my $fileC = new FileCatalog();

my ($user,$passwd) = $fileC->get_connection($SITE."::Admin");

if ( ! defined($user) ){  $user  = "";}
if ( ! defined($passwd) ){$passwd= "";}

if ( $user eq ""){
    print "Password : ";
    chomp($passwd = <STDIN>);
    $fileC->connect_as($SITE."::Admin","FC_admin",$passwd) || die "Connection failed for FC_admin\n";
} else {
    if ( $passwd eq "" ){
        print "Password for $user : ";
        chomp($passwd = <STDIN>);
    }
    $fileC->connect_as($SITE."::Admin",$user,$passwd)      || die "Connection failed for $user\n";
}


 my $listname = $ARGV[0];

 my @fileList = ();

  open (RUNLIST, $listname ) or die "cannot open $listname: $!\n";

  @fileList = <RUNLIST>;

  close(RUNLIST);

  my @prt = ();
  my @ppr = ();
  my $file;
  my $mpath;
  my $pathname;
  my $dpath;
  my $dnode;
  my @data;
  my $script;
  my $nn = 0;
  my $tophpss = "/home/starreco/reco";
  my $pathpass;

my $rscript = "/star/u/starreco/FCrmRootfiles_4.csh";

   open (CSFILE, ">$rscript");
  print CSFILE "#! /usr/local/bin/tcsh -f", "\n";

  foreach my $line (@fileList)  {
      chop $line;
  @prt = split("home", $line);
  $pathname = "/home".$prt[1];

  if($pathname =~ /st_fast/) {
  @ppr = split("st_fast", $pathname);
  $mpath = substr($ppr[0],0,-1);

  $file = "st_fast".$ppr[1]; 

  }elsif($pathname =~ /st_/ ) {

  @ppr = split("st_", $pathname);
  
  $mpath = substr($ppr[0],0,-1);

  $file = "st_".$ppr[1]; 

  } 
  $pathpass = $mpath;
  $pathpass =~ s/$tophpss//g;
#      print $pathpass, "\n";
  @prt = ();
  @data = ();  
  $fileC->set_context("path~$pathpass","filename=$file","available=1","storage!=hpss");
  @data = $fileC->run_query("path","node");

  if(scalar(@data) >= 1) {  
  foreach my $dline (@data)  {
#      print $dline, "\n";
  @prt = split("::",$dline);

   $dpath = $prt[0];
   $dnode = $prt[1];  
  }

  $nn++;
  $fileC->clear_context();


  $script = "/star/u/starreco/scripts/copy_".$nn.".csh";

  open (FILE, ">$script");
  print FILE "#! /usr/local/bin/tcsh -f", "\n";
  print FILE "pftp -v hpss.rcf.bnl.gov 4021 <<EOF","\n";
  print FILE "quote site setcos 11","\n";
  print FILE "bin","\n";
  print FILE "lcd ", $dpath, "\n";
  print FILE "cd ",$mpath,"\n";
  print FILE "pput ", $file, "\n";
  print FILE "__EOF__", "\n";
  print FILE "exit";

  close(FILE);
 `chmod +x $script`; 

  if($dnode =~ /rcas/ ) {

######################################################
      print $dnode, "\n";

     `ssh $dnode $script exit`;

     print "LOGOUT", "\n";
 }else{
     `$script`;
   print "Transfer from NFS", "\n";
  }

#
   }else{
 print CSFILE "fC_cleanup.pl -doit -mark off -cond path=",$mpath,",filename=",$file,",storage=hpss", "\n";

    }
  }

  close(CSFILE);
 `chmod +x $rscript`;

exit;
