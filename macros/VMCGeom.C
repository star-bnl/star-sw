void VMCGeom(Char_t *version="y2005x") {
  gSystem->Load("libTable.so");
  gInterpreter->ProcessLine(Form(".L %s.C",version));
  gInterpreter->ProcessLine(Form("%s()",version));
  if (! gGeoManager) return;
  TObjectSet *geom = new TObjectSet("Geometry",gGeoManager);
  geom->SetTitle(version);
  TFile *f = new TFile(Form("Geometry.%s.root",version),"recreate");
  geom->Write();
  delete f;
}
