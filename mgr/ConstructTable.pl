#!/usr/bin/env perl

use File::Basename;
my $input = shift;
my $file  = shift;

#print "ConstructTable :: $input $file\n";
if ($file =~ /Table\.h/) {
    my $idlHH   = $file; 
    #print "ConstructTable: $idlHH\n";
    if ($input =~ /\.idl/) {
	TableH($idlHH);
    } else {
	TableD($idlHH);
    }

} elsif ($file =~ /Table\.cxx/) {
    my $idlCXX  = $file; 
    #print "ConstructTable: $idlCXX\n";
    TableCXX($idlCXX);

} elsif ($file =~ /LinkDef\.h/) {
    my $LinkDef = $file; 
    #print "ConstructTable: $LinkDef\n";
    TableLinkDef($LinkDef);
}

#_____________________________________________________________________________
# do not change the comment format for the library version 
# It matches the one in stic and RootCint
sub tableH ($) {
    my $stem = $_[0];
    my $vers = 2;
    my $h = '
/*!
 * \class  St_'.$stem.'
 * \brief  C++ wrapper for <'.$stem.'> StAF table
 * \author Automatic Generation
 * \date   '.localtime().'
 *
 * This was generated for version \''.$ENV{STAR_VERSION}.'\'
 */
class St_' . $stem . ' : public TTable
{
 public:
   ClassDefTable(St_' . $stem . ',' . $stem . '_st)
   ClassDef(St_' . $stem . ',' . $vers . ') //C++ wrapper for <' . $stem . '> StAF table
};
#endif
';
print OUT $h;
}

#________________________________________
sub TableH {
    (my $dst = shift) =~ s/^\#//g; 
    my $dir = dirname($dst);
    rmkdir($dir);
    (my $stem = basename($dst,"_Table.h")) =~ s/^St_//g;
    #print "cons::TableH stem = $stem\n";
    open (OUT,">$dst") or die "Can't open $dst\n";
    my $h = '
#ifndef STAF_St_' . $stem . '_Table
#define STAF_St_' . $stem . '_Table

#include "TTable.h"

#include "' . $stem . '.h"
';
    print OUT $h;
    tableH($stem);
    close (OUT);
}
#________________________________________
sub TableD {
    (my $dst = shift) =~ s/^\#//g; 
    my $dir = dirname($dst);
    rmkdir($dir);
    (my $stem = basename($dst,"_Table.h")) =~ s/^St_//g;# print "cons::TableH stem = $stem\n";
    open (OUT,">$dst") or die "Can't open $dst\n";
  my $d = '
#ifndef STAF_St_' . $stem . '_Table
#define STAF_St_' . $stem . '_Table

#include "TTable.h"

#include "' . $stem . '.h"

typedef ' . $stem . ' ' . $stem . '_st;
';
    print OUT $d;
    tableH($stem);
    close (OUT);
}
#________________________________________
sub TableCXX {
    (my $dst = shift) =~ s/^\#//g; 
    (my $stem = basename($dst,"_Table.cxx")) =~ s/^St_//g;
    # print "cons::TableCXX stem = $stem\n";
    my $dir = dirname($dst);
    rmkdir($dir);
    open (OUT,">$dst") or die "Can't open $dst\n";

    my $c = '
#include "tables/St_' . $stem . '_Table.h"
/////////////////////////////////////////////////////////////////////////
//
//  Class St_' . $stem . ' wraps the STAF table ' . $stem . '
//  It has been generated "by automatic". Please don\'t change it "by hand"
//
/////////////////////////////////////////////////////////////////////////

#include "Stypes.h"
TableImpl(' . $stem . ')
';
    print OUT $c;
    close (OUT);
}

#________________________________________
sub TableLinkDef {
#  my $env = shift;
    my $dst = shift; 
    my $stem = basename($dst,"LinkDef.h");#  print "cons::TableLinkDef : $dst => $stem\n";
    my $dir = dirname($dst);
    rmkdir($dir);
    open (OUT,">$dst") or die "Can't open $dst\n";
    my $h = '
#ifdef __CINT__                       
#pragma link off all globals;         
#pragma link off all classes;         
#pragma link off all functions;       
#pragma link C++ class St_' . $stem . '-;     
#pragma link C++ class ' . $stem . '_st+;    
#endif 
';
    print OUT $h;                               
    close (OUT);
}

#____________________________________________________________
sub rmkdir {
    (my $Dir = $_[0]) =~ s/\\/\//g; 
    my @terms = split "/", $Dir; #print "Dir = $Dir => @terms\n";
    my $dir = "";
    foreach my $term (@terms) {
	if (!$dir) {next;}
	if ($dir) {$dir .= "/";}
	$dir .= $term; 
	#print "ConstructTable :: dir = $dir\n";
	if (!-d $dir && ! mkdir($dir,0755)) {
	    die "$0: can't create directory $path ($!).\n";
	}
    }
}

