#! /opt/star/bin/perl
use File::Basename;
my $input = shift;
my $file = shift;
if ($file =~ /Table\.h/) {
  my $idlHH   = $file; #print "ConstructTable: $idlHH\n";
  if ($input =~ /\.idl/) {
    TableH($idlHH);
  }
  else {
    TableD($idlHH);
  }
}
elsif ($file =~ /Table\.cxx/) {
my $idlCXX  = $file; #print "ConstructTable: $idlCXX\n";
  TableCXX($idlCXX);
}
elsif ($file =~ /LinkDef\.h/) {
  my $LinkDef = $file; #print "ConstructTable: $LinkDef\n";
  TableLinkDef($LinkDef);
}
#________________________________________________________________________________
sub tableH ($) {
  my $stem = $_[0];
  my $h = '
class St_' . $stem . ' : public St_Table
{
protected:
  static St_tableDescriptor *fgColDescriptors;
  virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}
  virtual void SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}
public:
  St_' . $stem . '() : St_Table("' . $stem . '",sizeof(' . $stem . '_st)) {SetType("' . $stem . '");}
  St_' . $stem . '(Text_t *name) : St_Table(name,sizeof(' . $stem . '_st)) {SetType("' . $stem . '");}
  St_' . $stem . '(Int_t n): St_Table("' . $stem . '",n,sizeof(' . $stem . '_st)) {SetType("' . $stem . '");}
  St_' . $stem . '(Text_t *name,Int_t n): St_Table(name,n,sizeof(' . $stem . '_st)) {SetType("' . $stem . '");}
  ' . $stem . '_st *GetTable(Int_t i=0) const { return ((' . $stem . '_st *)s_Table)+i;}
  ' . $stem . '_st &operator[](Int_t i){ assert(i>=0 && i < GetNRows()); return *GetTable(i); }
  const ' . $stem . '_st &operator[](Int_t i) const { assert(i>=0 && i < GetNRows()); 
						      return *((const ' . $stem . '_st *)(GetTable(i))); }
';
  print OUT $h;
  my $vers = 0;
 
  if ($stem eq "g2t_rch_hit") {$vers = 1;} 
  $h = '
    ClassDef(St_' . $stem . ',' . $vers . ') //C++ wrapper for <' . $stem . '> StAF table
};
#endif
';
print OUT $h;
}
#________________________________________
sub TableH {
#  my($env, $dst, $src) = shift;
#  my $env = shift;
  (my $dst = shift) =~ s/^\#//g; 
  my $dir = dirname($dst);
  rmkdir($dir);
  (my $stem = basename($dst,"_Table.h")) =~ s/^St_//g;# print "cons::TableH stem = $stem\n";
  open (OUT,">$dst") or die "Can't open $dst\n";
  my $h = '
#ifndef STAF_St_' . $stem . '_Table
#define STAF_St_' . $stem . '_Table

#include "St_Table.h"

#include "' . $stem . '.h"
';
  print OUT $h;
  tableH($stem);
  close (OUT);
}
#________________________________________
sub TableD {
#  my($env, $dst, $src) = shift;
#  my $env = shift;
  (my $dst = shift) =~ s/^\#//g; 
  my $dir = dirname($dst);
  rmkdir($dir);
  (my $stem = basename($dst,"_Table.h")) =~ s/^St_//g;# print "cons::TableH stem = $stem\n";
  open (OUT,">$dst") or die "Can't open $dst\n";
  my $d = '
#ifndef STAF_St_' . $stem . '_Table
#define STAF_St_' . $stem . '_Table

#include "St_Table.h"

#include "' . $stem . '.h"

typedef ' . $stem . ' ' . $stem . '_st;
';
  print OUT $d;
  tableH($stem);
  close (OUT);
}
#________________________________________
sub TableCXX {
#  my $env = shift;
  (my $dst = shift) =~ s/^\#//g; 
  (my $stem = basename($dst,"_Table.cxx")) =~ s/^St_//g;# print "cons::TableCXX stem = $stem\n";
  my $dir = dirname($dst);
  rmkdir($dir);
  open (OUT,">$dst") or die "Can't open $dst\n";
  if ($stem eq "g2t_rch_hit") {
    my $g2t_rch_hit_streamer = g2t_rch_hit_streamer();
    print OUT $g2t_rch_hit_streamer;
  }
  else {
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
}
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
#pragma link C++ class ' . $stem . '_st-!;    
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
    $dir .= $term; #print "dir = $dir\n";
    if (!-d $dir && ! mkdir($dir,0755)) {die "$0: can't create directory $path ($!).\n";}
  }
}
#____________________________________________________________
sub g2t_rch_hit_streamer {
  my $g2t_rch = '
#include "tables/St_g2t_rch_hit_Table.h"
/////////////////////////////////////////////////////////////////////////
//
//  Class St_g2t_rch_hit wraps the STAF table g2t_rch_hit
//  It has been generated "by automatic". Please don\'t change it "by hand"
//
/////////////////////////////////////////////////////////////////////////

#include "Stypes.h" 
#include "St_tableDescriptor.h"
#include "St_TableElementDescriptor.h"
//____________________________________________________________________________
  St_tableDescriptor *St_g2t_rch_hit::fgColDescriptors = 0;
  TableImp(g2t_rch_hit)

#define StreamElementIn(type)  case St_TableElementDescriptor::_NAME2_(k,type):            \
 if (nextCol->m_Dimensions)                                   \
   R__b.ReadStaticArray((_NAME2_(type,_t) *)(row+nextCol->m_Offset));                      \
 else                                                         \
   R__b >> *(_NAME2_(type,_t) *)(row+nextCol->m_Offset);      \
 break 

//____________________________________________________________________________
void St_g2t_rch_hit::Streamer(TBuffer &R__b) 
{
   if (!R__b.IsReading()) {
     R__b.WriteVersion(St_g2t_rch_hit::IsA());
     St_Table::Streamer(R__b); 
   } else {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      St_Table::StreamerTable(R__b);
      if (*s_MaxIndex <= 0) return; 
      char *row= s_Table;
      for (Int_t indx=0;indx<*s_MaxIndex;indx++,row += GetRowSize()) {
        tableDescriptor_st *nextCol = GetRowDescriptors()->GetTable();
        Int_t maxColumns = GetNumberOfColumns();
        for (Int_t colCounter=0; colCounter < maxColumns; nextCol++,colCounter++) 
        {
         //   Skip 5th elememnt
         //   float ds; /* energy deposition over ds */
          if (R__v == 0 && colCounter == 4) {
              *(Float_t *)(row+nextCol->m_Offset) = -1956.;
              continue; 
          }
          // Stream one table row supplied
          switch(nextCol->m_Type) {
           StreamElementIn(Float);
           StreamElementIn(Int);
           StreamElementIn(Long);
           StreamElementIn(Short);
           StreamElementIn(Double);
           StreamElementIn(UInt);
           StreamElementIn(ULong);
           StreamElementIn(UChar);
           StreamElementIn(Char);
          default:
            break;
         }
       }     
     }
   }
}

';
return $g2t_rch;
}
