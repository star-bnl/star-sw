void Ast2Rz(const Char_t *vers="y2005x", const Char_t *geom = "") {
  gROOT->LoadMacro("bfc.C");
  //  TString cmd("bfc(0,\"gstar,useXgeom,nodefault,");
  //  TString cmd("bfc(0,\"gstar,nodefault,StEvent,");
  //  TString cmd("bfc(0,\"gstar,ForceGeometry,nodefault,");
  TString cmd("bfc(0,\"geant,nodefault,");
  cmd += vers;
  cmd += ",";
  cmd += geom;
  cmd += "\")";
  cout << "cmd : " << cmd.Data() << endl;
  gInterpreter->ProcessLine(cmd.Data());
  St_geant_Maker *geant = (St_geant_Maker *) chain->Maker("geant");
  if (! geant) return;
  //  chain->Make();
  //  geant->Init();
  geant->InitRun(1);
  TString cmd("grfile"); 
  geant->Do(cmd.Data());
  //  geant->Do("FORTRAN/CLOSE 30");
#if 1
  ofstream out;
  TString fOut("Geometry.");
  fOut += vers;
  fOut += ".C";
  out.open(fOut.Data());
  out << "#include \"CreateGeometry.h\"" << endl;
  out << "TDataSet *CreateTable() {" << endl;
  geant->Version(out);
  out << "  return CreateGeometry(\"" << vers << "\");" << endl;
  out << "}" << endl;
  out.close(); 
  if (gSystem->Exec(Form("mv geom.rz %s.rz",vers))) gSystem->Exit(1);
#endif
}
