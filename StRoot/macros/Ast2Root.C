//#define OLD_GEANT_VMC
void Ast2Root(const Char_t *vers="y2005x") {
  gROOT->LoadMacro("bfc.C");
  //  TString cmd("bfc(0,\"gstar,useXgeom,nodefault,");
  TString cmd("bfc(0,\"gstar,nodefault,UseXgeom,");
  cmd += vers;
  cmd += "\")";
  cout << "cmd : " << cmd.Data() << endl;
  gInterpreter->ProcessLine(cmd.Data());
  St_geant_Maker *geant = (St_geant_Maker *) chain->Maker("geant");
  if (! geant) return;
  chain->Make();
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
}
