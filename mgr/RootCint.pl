#! /usr/local/bin/perl
#
# Script to run rootcint : 
#              Input     : list of h-files
#              Output    : 
#
use File::Basename;
#
#print "RootCint.pl ================\n";
my $Cint_cxx = shift;
(my $Cint_h  = $Cint_cxx) =~ s/_Cint\.cxx/_Cint\.h/g;
my $DirName = dirname($Cint_cxx); print "DirName = $DirName\n";
my $LinkDef = $DirName . "/" . "LinkDef.h"; 
print "Cint Files :", $Cint_cxx, ",", $Cint_h,",",$LinkDef,"\n";
my $sources  = shift;
#print "sources =", $sources,"\n";
my $CPPFLAGS = shift; #print "CPPFLAGS = ", $CPPFLAGS, "\n";
my @list_of_h_files = (); # List of h-files with ClassDef.
my @list_of_classes = (); # List of classes with ClassDef.
my @LINKDEF = (); # List of h-files with LinkDefs
for my $h  (split / /,$sources) {#  print "SRC:", $h, "\n";
  if ($h =~ /LinkDef/) {push @LINKDEF, $h;}# print "======================== Found LinkDef in $h\n";next;} 
  open (In,$h) or die "Can't open $h";
  my $h_ok = 0;
  my $dummy;
  my $ClassName;
  while ($line = <In>) {
    if ($line =~/ClassDef/) {
      if ($line =~ /\#\#/) {next;} # ClassDefs in macro
#      print $line;
      if (!$h_ok) {$h_ok++; push @list_of_h_files, basename($h);}
      my @words = split /([\(,\)])/, $line;
      my $ClassName = $words[2];
#      print "ClassName = ",$ClassName,"\n";
      if ($ClassName) { push  @list_of_classes, $ClassName;}
    }
    else {
      if ($line =~ /CollectionDef/) {
	print $line;
      }
      else {next;}
    }
  }
  close (In);
}
my $ListOfWrittenClasses = ""; 
if ($#list_of_classes > -1) {
  open (Out, ">$LinkDef") or die "Can't open $LinkDef";
  if ($#LINKDEF > -1) {
    for my $def (@LINKDEF) {
      open (In, $def) or die "Can't open $def";
      while ($line = <In>) {
	print Out $line; #print $line; 
	if ($line =~ / class / && $line  =~ /\#pragma/) {
	  my @words = split /([ \(,\)\;\-\!])/, $line;
	  my $i = 0;
#	  for my $word (@words) {print "word[",$i,"]=",$word,"\n"; $i++;}
	  $ListOfWrittenClasses .= ":" . $words[8]; #print "ListOfWrittenClasses = ",$ListOfWrittenClasses,"\n";
	}
      }
    }
    print Out "#ifdef __CINT__\n";                  #print  "#ifdef __CINT__\n";
  }  
  else {
    print Out "#ifdef __CINT__\n";                  #print  "#ifdef __CINT__\n";
    print Out "#pragma link off all globals;\n";    #print  "#pragma link off all globals;\n";
    print Out "#pragma link off all classes;\n";    #print  "#pragma link off all classes;\n";
    print Out "#pragma link off all functions;\n";  #print  "#pragma link off all functions;\n";
  }
  for $ClassName (@list_of_classes) {#    print "ClassName = $ClassName\n";
    if (!grep (/\:$ClassName/, $ListOfWrittenClasses)) {
      print Out "#pragma link C++ class $ClassName;\n"; #print  "#pragma link C++ class $ClassName;\n";
    }
  }
  print Out "#endif\n";                           #print  "#endif\n";
  close (Out);
}
my $files = join ' ', @list_of_h_files;
$files .= " " . "LinkDef.h"; print "files = ",$files,"\n";
my $local_cint = basename($Cint_cxx);
my $cmd  = "cd $DirName; rootcint -f $local_cint -c $CPPFLAGS $files";
print "cmd = ",$cmd,"\n";
my $flag = `$cmd`;
exit(0);
# last line
