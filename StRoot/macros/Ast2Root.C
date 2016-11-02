//#define OLD_GEANT_VMC
void Ast2Root(const Char_t *vers="y2016a", const Char_t *geom = "useXgeom") {
  gROOT->LoadMacro("bfc.C");
  //  TString cmd("bfc(0,\"gstar,useXgeom,nodefault,");
  TString cmd("bfc(0,\"gstar,nodefault,");
  cmd += vers;
  cmd += ",";
  cmd += geom;
  cmd += "\")";
  cout << "cmd : " << cmd.Data() << endl;
  gInterpreter->ProcessLine(cmd.Data());
  St_geant_Maker *geant = (St_geant_Maker *) chain->Maker("geant");
  if (! geant) return;
  //  chain->Make();
  geant->InitRun(1);
#if 0  
  TString rzFile(vers);
  rzFile += ".rz";
  TString cmd("grfile ");
  cmd += rzFile;
  geant->Do(cmd.Data());
  //  cmd = "g2Root ";
  cmd = "g2root ";
  rzFile.ReplaceAll("grfile ","");
  cmd += rzFile;
  cmd += " "; cmd += vers; cmd += ".h";
  gSystem->Exec(cmd);
#endif
  TVolume *hall = geant->GetDataSet("HALL");
  if (hall) {
    TFile *f = new TFile(Form("HALL.%s.root",vers),"recreate");
    hall->Write();
    delete f;
  }
  TString hfile(vers);
  hfile += ".h";
  geant->g2Root(hfile);
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
}
