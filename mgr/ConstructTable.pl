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
sub tableH ($) {
  my $stem = $_[0];
  print OUT "class St_",$stem," : public St_Table\n";
  print OUT "{\n";
  print OUT "protected:\n";
  print OUT "  static St_tableDescriptor *fgColDescriptors;\n";
  print OUT "  virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}\n";
  print OUT "  virtual void SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}\n";
  print OUT "public:\n";
  print OUT "  St_",$stem,"() : St_Table(\"",$stem,"\",sizeof(",$stem,"_st)) {SetType(\"",$stem,"\");}\n";
  print OUT "  St_",$stem,"(Text_t *name) : St_Table(name,sizeof(",$stem,"_st)) {SetType(\"",$stem,"\");}\n";
  print OUT "  St_",$stem,"(Int_t n): St_Table(\"",$stem,"\",n,sizeof(",$stem,"_st)) {SetType(\"",$stem,"\");}\n";
  print OUT "  St_",$stem,"(Text_t *name,Int_t n): St_Table(name,n,sizeof(",$stem,"_st)) {SetType(\"",$stem,"\");}\n";
  print OUT "  ",$stem,"_st *GetTable(Int_t i=0){ return ((",$stem,"_st *)s_Table)+i;}\n";
  print OUT "  ",$stem,"_st &operator[](Int_t i){ assert(i>=0 && i < GetNRows()); return *GetTable(i); }\n";
  print OUT "  const ",$stem,"_st &operator[](Int_t i) const { assert(i>=0 && i < GetNRows()); return *((const ",$stem,"_st *)(((St_",$stem," *)this)->GetTable(i))); }\n";
  print OUT "\n";
  print OUT "  ClassDef(St_",$stem,",0) //C++ wrapper for <",$stem,"> StAF table\n";
  print OUT "};\n";
  print OUT "\n";
  print OUT "#endif\n";
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
  print OUT "#ifndef STAF_St_",$stem,"_Table\n";
  print OUT "#define STAF_St_",$stem,"_Table\n";
  print OUT "\n";
  print OUT "#include \"St_Table.h\"\n";
  print OUT "\n";
  print OUT "#include \"",$stem,".h\"\n";
  print OUT "\n";
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
  print OUT "#ifndef STAF_St_",$stem,"_Table\n";
  print OUT "#define STAF_St_",$stem,"_Table\n";
  print OUT "\n";
  print OUT "#include \"St_Table.h\"\n";
  print OUT "\n";
  print OUT "#include \"",$stem,".h\"\n";
  print OUT "\n";
  print OUT "typedef ",$stem," ",$stem,"_st;\n";
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
  print OUT "#include \"tables/St_",$stem,"_Table.h\"\n";
  print OUT "/////////////////////////////////////////////////////////////////////////\n";
  print OUT "//\n";
  print OUT "//  Class St_",$stem," wraps the STAF table ",$stem,"\n";
  print OUT "//  It has been generated \"by automatic\". Please don't change it \"by hand\"\n";
  print OUT "//\n";
  print OUT "/////////////////////////////////////////////////////////////////////////\n";
  print OUT "\n";
  print OUT "#include \"Stypes.h\"\n";
  print OUT "TableImpl(",$stem,")\n";
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
  print OUT "#ifdef __CINT__\n";                       #print "#ifdef __CINT__\n";                       
  print OUT "#pragma link off all globals;\n";         #print "#pragma link off all globals;\n";         
  print OUT "#pragma link off all classes;\n";         #print "#pragma link off all classes;\n";         
  print OUT "#pragma link off all functions;\n";       #print "#pragma link off all functions;\n";       
  print OUT "#pragma link C++ class St_",$stem,"-;\n"; #print "#pragma link C++ class St_",$stem,"-;\n";     
  print OUT "#pragma link C++ class ",$stem,"_st-!;\n";#print "#pragma link C++ class ",$stem,"_st-!;\n";    
  print OUT "#endif\n";                                #print "#endif\n";                                
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
