#! /opt/star/bin/perl
#
# Script to run rootcint : 
#              Input     : list of h-files
#              Output    : 
#
use Env;
use File::Basename;
#
#print "RootCint.pl ================\n"; my $rootcint = `which rootcint`; 
#print "ROOTSYS = $ROOTSYS rootcint = $rootcint\n";
my $Cint_cxx = shift;
(my $Cint_h  = $Cint_cxx) =~ s/_Cint\.cxx/_Cint\.h/g;
my $DirName = dirname($Cint_cxx);# print "DirName = $DirName\n";
my $LinkDef = $DirName . "/" . "LinkDef.h"; #print "Cint Files :", $Cint_cxx, ",", $Cint_h,",",$LinkDef,"\n";
my $sources  = shift; #print "sources =", $sources,"\n";
my $CPPFLAGS = shift; #print "CPPFLAGS = ", $CPPFLAGS, "\n";
my %class_hfile = (); # class h-file map
my %class_hfile_depens_on = (); # 
my %class_written = (); 
my @classes = 0; # list of classes
my $h_files = "";
my $coll = 0;

# count no. of classes i LinkDef's
my $ListOfWrittenClasses = ":"; 
my $ListOfDefinedClasses = "";
my $off = 0;
open (Out, ">$LinkDef") or die "Can't open $LinkDef";
for my $def  (split /\s/,$sources) {#  print "SRC:", $def, "\n";
  if ($def =~ /LinkDef/ && !($def =~/^$LinkDef$/) ) {
    open (In, $def) or die "Can't open $def";
    while ($line = <In>) {
      print Out $line;# print $line; 
      if ($line =~ /link off all/) {$off++;}
      if ($line =~ / class / && $line  =~ /\#pragma/) {
	my @words = split /([ \(,\)\;\-\!])/, $line;
	my $class = $words[8];
	if ($class) {
	  push @classes, $class;
	  $class_written{$class} = "YES"; #print "class: $class, written: $class_written{$class}\n";
	}
      }
    }
  }
}
close (Out);
for my $h  (split /\s/,$sources) {#  print "SRC:", $h, "\n";
  next if !$h;
  if ($h =~ /Stypes/)  {$h_files .= " " . basename($h); next;}
  if ($h =~ /LinkDef/) {next;}
  open (In,$h) or die "Can't open $h";
  my $dummy;
  my $class;
  my $includes = "";
  while ($line = <In>) {
    if ($line =~ /\#include/ && $line !~ /(<>)/) {
      (my $inc_h = $line) =~ s/\#include\s//g; chop ($inc_h);
      $inc_h =~ s/\"//g; 
      # print "inc_h = $inc_h\n";
      my $inc_hh = basename($inc_h);
      if ($sources =~ /$inc_hh/) {
	$includes .= ":" . $inc_hh; #	print "--includes for $h: $includes\n";
      }
    }
    if ($line =~/ClassDef/) {
      if ($line =~ /\#\#/) {next;} # ClassDefs in macro definition
      my @words = split /([\(,\-\!\)])/, $line;
      my $class = $words[2];#      print "=================class = ",$class,"\n";
      if ($class) {
	push @classes, $class;
	$class_hfile{$class} = $h; #print "class: $class, written: $class_written{$class}, h: $class_hfile{$class}\n"; 
	$class_hfile_depens_on{$class} = $includes;
      }
    }
    if ($line =~ /\#define/) {next;}
    if ($line =~ /StCollectionDef/) {
      $coll++;  #print "$coll, $line\n";
    }
  }
  close (In);
  if ($coll) {# Collection Definition 
    my $macro = `echo \$STAR/StRoot/St_base/StArray.h`;
    my $tmp = "temp.h";
    open (INPUT, $h) or die "Can't open $h\n";
    my $new_h = $DirName . "/" . basename($h);
    open (OUTPUT, ">$tmp") or die "Can't open $tmp\n";
    while ($line = <INPUT>) {
      if ($line =~ /StCollectionDef/) {
	my @Class = split /([\(\)])/, $line;
	my $class = $Class[2];
	if ($class) {
	  (my $core = $class) =~ s/^St//g; # print "core $core\n";
          my $cl = "";
	  foreach my $stem ("Collection","Iterator","VecPtr") {
	    if ($stem eq "VecPtr") {$cl = "St" . $stem . $core . "-";}
	    else                   {$cl = "St" . $core . $stem . "-";}
	    push @classes, $cl; $class_hfile{$cl} = $new_h; $class_hfile_depens_on{$cl} = $includes;
#	    print "class: $stem $core $cl includes  $includes\n";
	  }
	  open(DEF, $macro) || die "Can't open Macros $macro \n";
	  my $def = 0;
	  while ($line = <DEF>) {
	    if ($line =~ /\#define/ && $line =~ /StCollectionDef/) {
	      $def = 1; next;
	    }
	    if ($def && $line =~ /\#define/) {last;}
	    if (! $def) { next; }
	    $line =~ s/\\//g;
	    $line =~ s/QWERTY/$class/g;
	    $line =~ s/ \#\# //g;
	    $line =~ s/\#\# //g;
	    $line =~ s/ \#\#//g;
	    print OUTPUT $line;#   print $line;
	  }
	}
      }
      else {print OUTPUT $line;}
    }
    close (OUTPUT);
    my $flag = rename $tmp, $new_h;  print "rename $tmp $new_h;\n";
  }
}
my $opened = "";
for my $class (@classes) {
  if ($class) {
    if (!$class_written{$class}) {
      if (!$opened) {
	open (Out,">>$LinkDef")  or die "Can't open $LinkDef";
	print Out "#ifdef __CINT__\n";                  print  "#ifdef __CINT__\n";
	if (! $off) {
	  print Out "#pragma link off all globals;\n";    print  "#pragma link off all globals;\n";
	  print Out "#pragma link off all classes;\n";    print  "#pragma link off all classes;\n";
	  print Out "#pragma link off all functions;\n";  print  "#pragma link off all functions;\n";
	}
	$opened = "YES";
      }
      if ($class_hfile{$class} =~ /_Module/) {
	print Out "#pragma link C++ class $class-;\n"; print  "#pragma link C++ class $class-;\n";
	(my $global = $class) =~ s/St_//g;
	print Out "#pragma link C++ global $global;\n"; print  "#pragma link C++ global $global;\n";
      }
      else {print Out "#pragma link C++ class $class;\n"; print  "#pragma link C++ class $class;\n";}
      $class_written{$class} = "YES";
    }
  }
}
if ($opened) {
  print Out "#endif\n";                           print  "#endif\n";
  close (Out);
  $opened = "";
}
if ($coll) { # order h-files with Collections 
  my $done = 0;
  while (!$done) {
    my $add = 0;
    for my $class (@classes) {#    print "list : $h_files\n";
      next if ! $class;
      my $h = $class_hfile{$class};
      next if ! $h;
      my $hh = basename($h); 
      next if $h_files =~ /$hh/; 
      my $hd =  $class_hfile_depens_on{$class};
      if ($hd) { my @hds = split /:/, $hd;#	       print "h: $h => $hd => @hds\n";
		 my $incsd = "";
		 foreach my $hdd (@hds) {
		   next if ! $hdd or $hdd eq " ";#print "hdd = $hdd h_files = $h_files\n";
		   next if $h_files =~ /$hdd/;
		   $incsd .= ":" . $hdd;
		   #		 print "parse $h: $hdd $incsd\n";
		 }
		 $hd = $incsd; #print "$h : depends on $hd\n";
		 $class_hfile_depens_on{$class} = $hd;#print "h: $h => $hd\n";
	       }
      #  print "class $class h = $h depends on $hd\n";# if $hd;
      next if $hd;
      $h_files .= " " . $hh;  $add++;# print "add $add : $h_files\n";
    }
    if (!$add) {$done = 1;}
  }
}
for my $class (@classes) {
  next if ! $class;
  my $h = $class_hfile{$class};#  print "Class: $class h: $h written: $class_written{$class} \n";
  if (!$h) {my $hfile = $DirName . "/" . $class . ".h"; #print "hfile = $hfile\n";
	    if (-f $hfile ) {$h = $hfile;} 
	  }
  if (!$h) {next;}
  my $hh = basename($h); #print "hh = $hh\n";
  if (!grep(/$hh/,$h_files)) {$h_files .= " " . $hh;}
}
my $hfile = $DirName . "/Stypes.h";
if (-f $hfile) {$h_files .= " Stypes.h";}
if ($h_files) {
  $h_files .= " " . "LinkDef.h";# print "files = ",$files,"\n";
  my $local_cint = basename($Cint_cxx);#  print "files = $#files\n";
  my $cmd  = "cd $DirName && rootcint -f $local_cint -c -DROOT_CINT -D__ROOT__ -I. $CPPFLAGS $h_files";
  print "cmd = ",$cmd,"\n";
  my $flag = `$cmd`;
}
else {
#    my $flag = `touch $Cint_cxx $Cint_h $LinkDef`;
}
exit(0);
# last line
