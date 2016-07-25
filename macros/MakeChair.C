/* 
   root.exe lDb.C 'MakeChair.C("tofTotbCorr")'
 */
void MakeChair(const Char_t *Name = "tpcRDOMasks") {
  TString TName("St_");
  TName += Name;
  if ( gClassTable->GetID(TName) < 0) {
    cout << "Dictionary for class " << TName << " has not been loaded. Execute gSystem->Load(\"libStDb_Tables.so\");" << endl;
    return;
  }
  TTable *table = TTable::New(Name,Name,0,0);
  table->Print();
  TTableDescriptor *dscT = table->GetRowDescriptors();
  //  dscT->Print();
  if (!dscT ) {cout << " table has no descriptor" << endl; return;}
  TTableDescriptor::iterator dsc  = dscT->begin();
  TTableDescriptor::iterator dscE = dscT->end();
  ofstream out;
  TString fOut(TName);
  fOut += "C.h";
  Char_t *file = gSystem->Which(".",fOut,kReadPermission);
  if (file) {cout << file << " has been already created" << endl; return;}
  
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "#ifndef St_" << Name << "C_h" << endl;
  out << "#define St_" << Name << "C_h" << endl;
  out << "" << endl;
  out << "#include \"TChair.h\"" << endl;
  out << "#include \"tables/St_" << Name << "_Table.h\"" << endl;
  out << "" << endl;
  out << "class St_" << Name << "C : public TChair {" << endl;
  out << " public:" << endl;
  out << "  static St_" << Name << "C* \tinstance();" << endl;
  out << "  " << Name << "_st \t*Struct(Int_t i = 0) \tconst {return ((St_" << Name << "*) Table())->GetTable()+i;}" << endl;
  out << "  UInt_t     \tgetNumRows()                \tconst {return GetNRows();}" << endl;
  for (;dsc != dscE; dsc++) {
    TString name = TTable::GetTypeName(dsc->fType);
    name.ReplaceAll("unsigned int","UInt_t");
    name.ReplaceAll("int","Int_t");
    name.ReplaceAll("unsigned char","UChar_t");
    name.ReplaceAll("char","Char_t");
    name.ReplaceAll("float","Float_t");
    name.ReplaceAll("double","Double_t");
    name.ReplaceAll("unsigned short","Ushort ");
    name.ReplaceAll("short","Short_t");
    for (Int_t i = 0; i < 3; i++) {if (! dsc->fIndexArray[i]) break; name += "*";}
    cout << '\t'<< name.Data() << '\t'<< dsc->fColumnName << endl;
    out << "  " << name << " \t" <<  dsc->fColumnName << "(Int_t i = 0) \tconst {return Struct(i)->" << dsc->fColumnName << ";}" << endl;
  }
  out << " protected:" << endl;
  out << "  St_" << Name << "C(St_" << Name << " *table=0) : TChair(table) {}" << endl;
  out << "  virtual ~St_" << Name << "C() {fgInstance = 0;}" << endl;
  out << " private:" << endl;
  out << "  static St_" << Name << "C* fgInstance;" << endl;
  out << "  ClassDefChair(St_" << Name << ", " << Name << "_st )" << endl;
  out << "  ClassDef(St_" << Name << "C,1) //C++ TChair for " << Name << " table class" << endl;
  out << "};" << endl;
  out << "#endif" << endl;
  out.close(); 
}
