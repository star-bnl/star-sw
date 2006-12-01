#define OLD_GEANT_VMC
void Ast2Root(const Char_t *vers="y2005x") {
  gROOT->LoadMacro("bfc.C");
  TString cmd("bfc(0,\"gstar,nodefault,");
  cmd += vers;
  cmd += "\")";
  gInterpreter->ProcessLine(cmd.Data());
  geant = (St_geant_Maker *) chain->Maker("geant");
  if (! geant) return;
#ifndef OLD_GEANT_VMC
  geant->DetSetIndex();
#endif
  TString rzFile(vers);
  rzFile += ".rz";
  TString cmd("grfile ");
  cmd += rzFile;
  geant->Do(cmd.Data());
#ifdef OLD_GEANT_VMC
  Char_t *sets[2] = {"geom","Detectors"};
  for (Int_t i = 0; i < 2; i++) {
    TString setN(".const/");
    setN += sets[i];
    TDataSet *set = geant->Find(setN.Data());
    if (set) {
      TString file(sets[i]);
      file += ".";
      file += vers;
      file += ".root";
      TFile *f = new TFile(file.Data(),"RECREATE");
      set->Write();
      delete f;
    }
  }
#endif
  cmd = "g2Root ";
  rzFile.ReplaceAll("grfile ","");
  cmd += rzFile;
  cmd += " "; cmd += vers; cmd += ".h";
  gSystem->Exec(cmd);
}
