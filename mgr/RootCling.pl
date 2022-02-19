#!/usr/bin/env perl
#
# Script to run rootcling $Lib $libDep $RootMap Cint_cxx list of h-files
#              Input     : list of h-files
#              Output    :
#
use Env;
use File::Basename;
use Cwd;
#
my $Lib = cwd(); $
Lib .= "/";
$Lib .= shift;
$LibDep = shift;
$RootMap = shift;
my $Cint_cxx = shift;
my $Cint_h  = $Cint_cxx;
$Cint_h  =~ s/_Cint\.cxx/_Cint\.h/g;
print "Rootcling.pl :\n Lib = $Lib,\n LibDep = $LibDep,\n RootMap = $RootMap\n Cint_cxx = $Cint_cxx,\n h_files = @ARGV\n";

my $DirName = dirname($Cint_cxx);		#print "DirName = $DirName\n";
my $LinkDef = $DirName . "/" . "LinkDef.h"; 	#print "Cint Files :", $Cint_cxx, ",", $Cint_h,",",$LinkDef,"\n";

my $LinkDefDirName;

my $sources  = shift; 				#print "sources =", $sources,"\n";
my $CPPFLAGS = shift; 				#print "CPPFLAGS = ", $CPPFLAGS, "\n";
#my @cpps = split / /,$CPPFLAGS;			#print "cpps: @cpps \n";

my %class_hfile = (); 		# class h-file map
my %class_hfile_depens_on = (); #
my %class_written = ();
my @classes = 0; 		# list of classes
my %namespaces = ();            # hash of namespaces
my $h_files = "";
my $h_dir   = "";
my $coll = 0;
my $col  = 0;
my $IncDirName = "";
# count no. of classes in LinkDef's
my $ListOfWrittenClasses = ":";
my $ListOfDefinedClasses = "";
my $off = 0;

open (Out, ">$LinkDef") or die "Can't open $LinkDef";

# do not change the comment format  for the library version 
# It matches the one in stic and ConstructTable
print Out "/* This was generated for version '".$ENV{STAR_VERSION}."' */\n";
#print Out "#if defined(__CINT__) && !defined(__ROOTCLING__)\n";
print Out "#pragma link off all globals;\n";
print Out "#pragma link off all classes;\n";
print Out "#pragma link off all functions;\n";;
#if ($Lib !~ /StarClassLibrary1/) {
my $off = 1;
for my $def  (split /\s/,$sources) {		#print "SRC:", $def, "\n";
  if (!($def =~ /LinkDef/  ))		{next;}
  if ( ($def =~/^$LinkDef$/))   	{next;}
  open (In, $def) or die "Can't open $def";
  $LinkDefDirName = dirname($def);
  my $openedBracket = 0; # for #ifndef __CLING__
  my $openedelse = 0;
  while ($line = <In>) {
    #    print "$line";
    #    if ($openedBracket && $line =~ /^\#else/) {$openedBracket = 0; $openedelse = 1; next;}
    #    if (($openedBracket || $openedelse) && $line =~ /^\#endif/) {$openedBracket = 0; next;}
    #    if ($line =~ /^\#ifndef __CLING__/) {$openedBracket = 1; next;}
    #    if ($openedBracket) {next;}
    if (($line  =~ /^\/\/IncFile *=/))	{
      my @words = split /(=)/, $line;
      chomp(@words[2]);
      $h_files .= " " . @words[2];
      next;
    }
    if (!($line  =~ /^\#pragma/))	{goto PRINT;}
    if ($line =~ /link off all/) 	{next;}
    if (!($line =~ / class / ))		{goto PRINT;}
    my @words = split /([ \(,\)\;\-\!+])/, $line;
    if ($words[10] != "class") 		{goto PRINT;}
    my $class = "";
    for (my $i = 12; $i < $#words; $i++) {
      next if $words[$i] eq '' or $words[$i] =~ /^\s+/; 
      $class = $words[$i]; 
      last;
    } 
    my $classG = $class;
    if ($classG =~ /</) {($classG) = split '<', $classG;} 
    if (!$classG) 			{goto PRINT;}
    if ($classG =~ /::/) {$classG =~ s/.*:://;}
    if ($class_written{$class} and $class eq $classG)		{next;}
    push @classes, $classG;
    $class_written{$classG} = 1; 	#print "class: $class, written: $class_written{$class}\n";
  PRINT: 
    print Out $line;
    #    print "Out: $line";
  }
}
close (Out);

for my $h  (split /\s/,$sources) {	
    #print "DEBUG SRC:", $h, "\n";
    next if !$h;
    next if $h =~ /LinkDef/;
    if ($h =~ /Stypes/)  {
      $h_files .= " " . basename($h); 
      next;
    } 
    $h_dir = dirname($h);
    my $hh = $h;
    if (!-f $hh) {($hh = $h) =~ s/.*.share/StRoot/;}
    if (!-f $hh) {($hh = $h) =~ s/.*.share/asps/;}
    if (!-f $hh) {($hh = $h) =~ s/.*.share//;}
    open (In,$hh) or die "Can't open $hh";
    my $dummy;
    my $class;
    my $includes = "";
    my $com = 0;
    my $nmspc = "";
    while ($line = <In>) {
      chomp($line);
      if ( $line =~ /\/\/\*/){
	# will not try to analyze this one and assume //.*
	$line =~ s/\/\/\*//;
      }
      if ($com) {
	if ($line =~ /\*\//) {
	  $com = 0; 
	  $line =~ s/^*\*\///; # print "==> $line";
	} else {
	  next;
	}
      }
      if ($line =~ m/(.*)(\/\*)/ ) {
	$Lprefix = $1;
	# be sure we do not have a /* preceded by a //
	# the //.* will be removed later
	if ( $Lprefix !~ m/\/\// ){ #|| ! $NEW ){
	  #print "DEBUG: Enabling comment mode\n" if ($rdebug);
	  $com = 1;
	  if ($line =~ /\*\//) {
	    $line =~ s/\/\*.*\*\///;
	    $com = 0;
	  } else {
	    $line =~ s/\/\*.*$//;
	  }
	}
      }
      #  next if ($com);
      
      
      # JL 2011 - weak parsing - should detect brackets etc ...
      #           but a first try - tor turn off, name "{" on the same
      #           line than namespace and for enabling, on the line afterward
      if ($line =~ /(namespace\s+)(\w+)/ ){
	$dummy = $2;
	if ( $line !~ /using/ && $line =~ m/\$NMSPC/ ){
	  $nmspc = $dummy;
	}
	#print "DEBUG: YEAH! Matched\n";
      } else {
	#print "DEBUG: Did not match [$line]\n";
      }
      
      #if ( $NEW){
      # strip of // should likely happen after /* */
      if ($line =~ /\/\//) {
	$line =~ s/\/\/.*$//;
	# print "==> $line";
      }
      #}
      
      # print "DEBUG: <$line>\n" if ($rdebug);
      
      if ($line =~ /\#include/ && $line !~ /(<>)/ && $line !~ /Table\.h/) {
	(my $inc_h = $line) =~ s/\#include\s//g; chop ($inc_h);
	$inc_h =~ s/\"//g; 
	# print "inc_h = $inc_h\n";
	
	my $inc_hh = basename($inc_h);
	if ($sources =~ /$inc_hh/) {
	  $includes .= ":" . $inc_hh;   	
	  # print "--includes for $h: $includes\n";
	}
      }
      
      if ($line =~/ClassDef/ && $line !~ /ClassDefChair/) { 
	# print "================================ $line \n";
	if ($line =~ /\#\#/) { next;} # ClassDefs in macro definition
	my @words = split /([\(,\-\!\)])/, $line;
	my $class = $words[2];      	
	#print "=================class = ",$class," ($h_dir)\n"; 
	if ( index($IncDirName,$h_dir) == -1 && $h_dir ne $DirName){
	  # print "=> Adding $h_dir to [$IncDirname]\n";
	  $IncDirName .= "-I$h_dir ";
	}
	
	if ($class) {
	  # print "Note - pushing [$nmspc] [$class] from $h\n" if ($nmspc ne "");
	  $nclass = $nmspc eq ""?$class:$nmspc."::".$class;
	  #$nclass = $class;
	  push @classes, $nclass;
	  $class_hfile{$nclass} = $h; 	
	  # print "class: $class, written: $class_written{$class}, h: $class_hfile{$class}\n";
	  
	  $class_hfile_depens_on{$nclass} = $includes;
	  
	  $namespaces{"$nmspc"}++ if ( $nmspc ne "" );
	}
      }
      if ($line =~ /\#define/) { next;}
      if ($line =~ /StCollectionDef/) {
	$coll++;  #print "$coll, $line\n";
      }
    }
    close (In);
    
    
    if ($coll) {			# Collection Definition
      my $macro = "./StRoot/St_base/StArray.h";
      if (-f $macro) {}
      else {$macro = `echo \$STAR/StRoot/St_base/StArray.h`;}
      my $tmp = "/tmp/temp" . $$ . ".h";
      open (INPUT, $h) or die "Can't open $h\n";
      my $new_h = $DirName . "/" . basename($h);
      open (OUTPUT, ">$tmp") or die "Can't open $tmp\n";
      while ($line = <INPUT>) {
	if ($line =~ /StCollectionDef/) {
	  my @Class = split /([\(\)])/, $line;
	  my $class = $Class[2];
	  if ($class) {
	    (my $core = $class) =~ s/^St//g; 		#print "core $core\n";
	    my $cl = "";
	    foreach my $stem ("Iterator","PtrVec","SPtrVec") {
	      if ($stem eq "Iterator") {$cl = "St" . $core . $stem      ;}
	      else                     {$cl = "St" . $stem . $core . "-";}
	      #print "DEBUG pushing $cl\n";
	      push @classes, $cl;
	      $class_hfile{$cl} = $new_h; $class_hfile_depens_on{$cl} = $includes;
	      #print "class: $stem $core $cl includes  $includes\n";
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
	      print OUTPUT $line;				#print $line;
	    }
	  }
	}
	else {print OUTPUT $line;}
      }
      close (OUTPUT);
#      print "rename $tmp, $new_h\n";
      rename $tmp, $new_h or 
	`mv $tmp $new_h`;# or die "Can't rename or move $tmp to $new_h;\n";
      
    }
  } #end Collection Definition
#}

my $opened = "";
foreach my $class (@classes) {
  #  if ($class =~ /Iterator/ || $class =~ /PtrVec/ ||
  #      $class =~ /SPtrVec/) {$class_written{$class} = "YES";}
  if ($class) {
    if (!$class_written{$class}) {
      if (!$opened) {
	open (Out,">>$LinkDef")  or die "Can't open $LinkDef";
#	print Out "#ifdef __CINT__\n";                  #print  "#ifdef __CINT__\n";
	if (! $off) {
	  print Out "#pragma link off all globals;\n";    #print  "#pragma link off all globals;\n";
	  print Out "#pragma link off all classes;\n";    #print  "#pragma link off all classes;\n";
	  print Out "#pragma link off all functions;\n";  #print  "#pragma link off all functions;\n";
	}
	# Masa hints - put those prior to Name::Class
	foreach my $name (sort keys %namespaces){
	  my $count = $namespaces{"$name"};
	  print     "#pragma link C++ namespace $name;\n";
	  print Out "#pragma link C++ namespace $name;\n";
	}
	$opened = "YES";
      }
      if ($class_hfile{$class} =~ /_Module/) {
	print Out "#pragma link C++ class $class-;\n"; 
	print     "#pragma link C++ class $class-;\n";
	(my $global = $class) =~ s/St_//g;
	#  vf print Out "#pragma link C++ global $global;\n"; print  "#pragma link C++ global $global;\n";
      } else {
	if ($class =~ /^St/ and $class =~ /Iterator$/) {
	  print Out "#pragma link C++ typedef $class;\n"; 
	  print     "#pragma link C++ typedef $class;\n";
	} else {
	  # print "======> |$class|\n";
	  if ($class =~ /-$/) {
	    print Out "#pragma link C++ class $class;\n"; 
	    print     "#pragma link C++ class $class;\n";
	  } else {
	    print Out "#pragma link C++ class $class+;\n"; 
	    print     "#pragma link C++ class $class+;\n";
	  }
	}
      }
      $class_written{$class} = "YES";
    }
  }
}

if ($opened) {
#  print Out "#endif\n";                           	#print  "#endif\n";
  close (Out);
  $opened = "";
}

if ($coll) { # order h-files with Collections
  my $done = 0;
  while (!$done) {
    my $add = 0;
    for my $class (@classes) {				#print "list : $h_files\n";
      next if ! $class;
      my $h = $class_hfile{$class};
      next if ! $h;
      my $hh = " " . basename($h) . " ";
      next if $h_files =~ /$hh/;
      my $hd =  $class_hfile_depens_on{$class};
      if ($hd) { my @hds = split /:/, $hd; 	       	#print "h: $h => $hd => @hds\n";
		 my $incsd = "";
		 foreach my $hdd (@hds) {
		   next if ! $hdd or $hdd eq " ";	#print "hdd = $hdd h_files = $h_files\n";
		   next if $h_files =~ /$hdd/;
		   $incsd .= ":" . $hdd;		#print "parse $h: $hdd $incsd\n";
		 }
		 $hd = $incsd; 				#print "$h : depends on $hd\n";
		 $class_hfile_depens_on{$class} = $hd;	#print "h: $h => $hd\n";
	       }
      #print "class $class h = $h depends on $hd\n";# if $hd;
      next if $hd;
      $h_files .= $hh; $add++;			#print "add $hh : $h_files\n";
    }
    if (!$add) {$done = 1;}
  }
}

for my $class (@classes) {	#loop over classes
  next if ! $class;
  my $h = $class_hfile{$class};  			#print "Class: $class h: $h written: $class_written{$class} \n";
  foreach my $ext ((".h",".hh")){                       #search for a few
    if (!$h) {	#No .h for class
      my $hfile = $DirName . "/" . $class . $ext; 	#print "1 hfile = $hfile\n";
      if (! -f $hfile) {   $hfile =$LinkDefDirName . "/" . $class . $ext;
			   #print "2 hfile = $hfile\n";}
			 }
      if (  -f $hfile) {   $h = $hfile;}
    }
    if ($h) { last;}
  }
  if (!$h) {
    ##      print STDERR "Rootcling.pl :: Warning : $class.h(h) NOT FOUND\n";
    next;
  }
  
  my $hh = " " . basename($h) . " "; 				#print "hh = $hh\n";
  if ($h_files !~ /$hh/ )  {$h_files .= $hh;}
}#end loop over classes

my @h_files = split ' ', $h_files;
my $h_filesC = "";
my $h_filesR = "";
foreach my $h (@h_files) {
  if ($h =~ /Collection/) {$h_filesC .= $h . " ";}
  else                    {$h_filesR .= $h . " ";}
}
$h_files = $h_filesC . " " . $h_filesR;
# print "h_files= $h_files\n";
my $hfile = $DirName . "/Stypes.h";
if (-f $hfile) {$h_files .= " Stypes.h";}
if ($h_files) {
  $h_files .= " " . "LinkDef.h";
#  $CPPFLAGS = " -I" . $DirName . " " . $IncDirName . $CPPFLAGS;
  $CPPFLAGS = " -I" . cwd() . "/" . $DirName . " " . $IncDirName . $CPPFLAGS;
#  my $cmd  = "rootcling -rootbuild -f $Cint_cxx -rml $Lib -rmf $RootMap";
#  my $cmd  = "rootcling -rootbuild -f $Cint_cxx -s $Lib -rml $Lib -rmf $RootMap";
#  my $cmd  = "rootcling -rootbuild -f $Cint_cxx -cxxmodule  -s $Lib";
#  my $cmd  = "rootcling -rootbuild -f $Cint_cxx -s $Lib";
#  my $cmd  = "rootcling -rootbuild -f $Cint_cxx -cxxmodule -s $Lib -rmf $RootMap";
  $ENV{ROOTIGNOREPREFIX} = 1;
  $ENV{ROOT_MODULES} = 1;
  my $cmd  = "rootcling -rootbuild -f $Cint_cxx";
#  $cmd .= " -cxxmodule";
#  $cmd .= " -inlineInputHeader -s $Lib -rmf $RootMap";
  my $bLib = File::Basename::basename($Lib);
#  my $RootMapF =   cwd() . "/" . $RootMap;
  $cmd .= " -inlineInputHeader -s $Lib -rml $bLib -rmf $RootMap";
#  $cmd .= " -inlineInputHeader -s $Lib -rml $bLib -rmf $RootMapF";
#  my $LibF = cwd() . $Lib;
#  $cmd .= " -inlineInputHeader -s $Lib -rml $bLib -rmf $RootMap";
  if ($LibDep) {
    my @libs = split(' ',$LibDep);
    foreach my $l (@libs) {
      #	print "l = $l\n";
      my $m = $l;
      $m =~ s/-l/lib/;
      $m .= "_rdict.pcm";
      $cmd .= " -m " . $m;
    }
  }
#  $cmd .= " -c -p -DROOT_CINT -D__ROOT__ $CPPFLAGS $h_files";
#  $cmd .= " -excludePath $ROOT/$ROOT_LEVEL/root -excludePath $ROOTSYS -excludePath $STAR -excludePath $STAR/.$STAR_HOST_SYS/include"; 
  $cmd .= " -DROOT_CINT -D__ROOT__ $CPPFLAGS $h_files";
  print "cmd (normal)= ",$cmd,"\n";
  my $flag = `$cmd`; if ($?) {exit 2;} 
  my $LIBDIR = File::Basename::dirname($Lib);
  my $pcmfile = File::Basename::basename($Lib);
  $pcmfile =~ s/\.so/_rdict\.pcm/;
  my $OBJDIR = File::Basename::dirname($Cint_cxx);
#  $cmd = "ln -s $LIBDIR/$pcmfile $OBJDIR/$pcmfile"; print "cmd $cmd\n";
#  $flag = `$cmd`; if ($?) {exit 2;}
  link $LIBDIR."/".$pcmfile, $OBJDIR."/".$pcmfile
}
exit(0);
# last line
