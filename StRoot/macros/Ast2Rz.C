void Ast2Rz(const Char_t *vers="y2005x", const Char_t *geom = "") {
  gROOT->LoadMacro("bfc.C");
  //  TString cmd("bfc(0,\"gstar,useXgeom,nodefault,");
  TString cmd("bfc(0,\"gstar,nodefault,StEvent,");
  cmd += vers;
  cmd += ",";
  cmd += geom;
  cmd += "\")";
  cout << "cmd : " << cmd.Data() << endl;
  gInterpreter->ProcessLine(cmd.Data());
  St_geant_Maker *geant = (St_geant_Maker *) chain->Maker("geant");
  if (! geant) return;
  chain->Make();
  TString cmd("grfile rzfile.rz"); 
  geant->Do(cmd.Data());
}
