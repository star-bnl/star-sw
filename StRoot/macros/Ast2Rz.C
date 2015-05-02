void Ast2Rz(const Char_t *vers="y2005x", const Char_t *geom = "") {
  gROOT->LoadMacro("bfc.C");
  //  TString cmd("bfc(0,\"gstar,useXgeom,nodefault,");
  //  TString cmd("bfc(0,\"gstar,nodefault,StEvent,");
  TString cmd("bfc(0,\"gstar,ForceGeometry,nodefault,");
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
}
