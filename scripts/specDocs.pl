#!/usr/bin/perl -w

# Updates special documentation files if necessary
# Jan  7, 2008 - Written by Gene Van Buren, converted from the old
#   doc.pm CGI module into a cron script

# write_area is the scratch area in which the script is allowed to work
# web_area is the directory where files will be made available on the web
#
# If any new libraries are added, be sure to put additional entries in
# the names, sdirs, and makepdf tables below.

use File::Path;
use File::Copy;

$web_area = "\/afs\/rhic.bnl.gov\/star\/doc_public\/www\/html\/tmp\/";

# verify this ENV exists
if ( ! defined($ENV{SCRATCH}) ){  $ENV{SCRATCH} = "/tmp/".$ENV{USER};}
$write_area = $ENV{SCRATCH} . "\/specDocs\/";

# possible combinations of documentation
@doclibs = ("StEvent","StMcEvent","StarClassLibrary","StAssociationMaker",
            "StTrsMaker","StSecondaryVertexMaker");
@levels = ("CVS","dev","new","pro");
@types = ("pdf","ps");


# create dir if it does not exists - start at level 11
if ( ! -d $write_area ){
    my(@items)=split("/",$write_area);
    my($p)="/".$items[0];
    for ($i=1; $i <= $#items ; $i++){
	$p .= "/".$items[$i];
	if ( ! -d $p){  mkdir($p,0700); }
    }
}



# save output in a log file
$logfile = $write_area . "specDocs.log";
open(STDERR, ">>$logfile") or die "Unable to append to ${logfile}\n";
if (`/bin/grep Fail $logfile | /usr/bin/wc -l` > 0) {
  `/bin/mail -s 'DOC_LOG_FAIL' gene\@bnl.gov < $logfile`;
}

# These are the file names used for the .tex and .ps/.pdf files.
%names = (
   "StEvent"                => "StEvent",
   "StMcEvent"              => "StMcEvent",
   "StarClassLibrary"       => "StarClassLibrary",
   "StAssociationMaker"     => "StAssociationMaker",
   "StTrsMaker"             => "trs",
   "StSecondaryVertexMaker" => "docXiFinder",
);

# Subdirectories for the documentation
%subdirs = (
   "StEvent"                => "tex\/",
   "StMcEvent"              => "tex\/",
   "StarClassLibrary"       => "tex\/",
   "StAssociationMaker"     => "tex\/",
   "StTrsMaker"             => "",
   "StSecondaryVertexMaker" => "",
);

# If the Makefile for this library allows building pdf files, enter a 1 here.
%makepdf = (
   "StEvent"                => 0,
   "StMcEvent"              => 0,
   "StarClassLibrary"       => 0,
   "StAssociationMaker"     => 0,
   "StTrsMaker"             => 0,
   "StSecondaryVertexMaker" => 0,
);

#----------------

$packdir = "\/afs\/rhic.bnl.gov\/star\/packages\/";

(-d $write_area) or mkpath($write_area);
chdir($write_area);
rmtree("StRoot") if (-d "StRoot");

foreach $level (@levels) {
  foreach $doclib (@doclibs) {

    $subdir = "StRoot\/${doclib}\/doc\/" . $subdirs{$doclib};

    if ($level eq "CVS") {
	$SR2 = $packdir . "repository";
	$ENV{CVSROOT} = $SR2;
	$SR = $SR2 . "\/";
	$thesource = $SR . $subdir . $names{$doclib} . ".tex,v";
    } else {
	$SR = $packdir . $level . "\/";
	$thesource = $SR . $subdir . $names{$doclib} . ".tex";
    }

    foreach $type (@types) {

      $thefile = $names{$doclib} . "." . $type;
      $psfile = $names{$doclib} . ".ps";
      $thefile2 = $level . "_" . $thefile;
      $psfile2 = $level . "_" . $psfile;
      $thefile3 = $web_area . $thefile2;
      $psfile3 = $web_area . $psfile2;

      $createnew = 0;

      if (-e $thefile3) {
	if ((-M $thefile3) > (-M $thesource)) {
	    unlink($thefile3);
	    $createnew = 1;
	}
      } else {
	if ((($type eq "pdf") && (! $makepdf{$doclib})) && (-e $psfile3)) {
	    `/usr/bin/ps2pdf $psfile3 $thefile3`;
	    (-e $thefile3) or die "Failed to convert $psfile3 to $thefile3.\n";
	} else {
	    $createnew = 1;
	}
      }
      if ($createnew) {
        if ($level eq "CVS") {
	    `/usr/bin/cvs co $subdir`;
	} else {
	    mkpath($subdir) || die "Could not make $subdir in $write_area\n";
	}
	(-e $subdir) or die "Failed to find/create subdirectory $subdir.\n";
	chdir($subdir);
        if ($level ne "CVS") {
	    $olddir = $SR . $subdir . "\*";
            # File::Copy::Recursive not available, and not
            #   worth implementing here just for one call
            `/bin/cp -R $olddir .`;
	}
	if ($type eq "pdf") {
	    if ($makepdf{$doclib}) {
		`/usr/bin/make -s pdf`;
	    } else {
		`/usr/bin/make -s`;
		`/usr/bin/ps2pdf $psfile $thefile`;
                move($psfile,$psfile3);
	    }
	} else {
	    `/usr/bin/make -s`;
	}
	(-e $thefile) or die "Failed to create file $thefile.\n";
        move($thefile,$thefile3);
      }

    }

  }
  chdir($write_area);
  rmtree("StRoot") if (-d "StRoot");

}


##########
# Rotate log file

if ( (-e $logfile) and (-s $logfile > 100000)) {
    $zipf = $logfile . ".gz.";
    $zipn = `/bin/ls -1 ${zipf}* | /usr/bin/wc -l`;
    chomp $zipn;
    $zipn =~ s/ //g;
    while ($zipn >= 0) {
      $zipfname = $zipf . $zipn;
      if ($zipn >= 9) {
        unlink($zipfname);
      } else {
        $zipfname2 = $zipf . ($zipn + 1);
        move($zipfname,$zipfname2) if (-e $zipfname);
      }
      $zipn--;
    }
    `/usr/bin/gzip $logfile -c > $zipfname;`;
    unlink($logfile);
}

close(STDERR);

exit;

#_____________________________________________________________________________
# $Id: specDocs.pl,v 1.4 2008/01/16 18:35:19 jeromel Exp $
# $Log: specDocs.pl,v $
# Revision 1.4  2008/01/16 18:35:19  jeromel
# Fix for ENV not existing within cron (SCRATCH is part of the STAR login whihc is NOT executed if a perl script is directly accessed within a cron)
#
# Revision 1.3  2008/01/16 18:08:18  jeromel
# Create missing directory
#
# Revision 1.2  2008/01/16 18:03:05  jeromel
# Remove ^M characters everywhere
#
# Revision 1.1  2008/01/11 23:54:07  genevb
# Introduction of specDocs.pl
#
#

