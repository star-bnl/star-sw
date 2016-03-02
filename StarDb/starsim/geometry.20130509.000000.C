
TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  St_geant_Maker *geant = (St_geant_Maker *) StMaker::GetChain()->Maker("geant");
  if (! geant) return 0;
  TDataSet *geom = new TDataSet("geometry");
  geant->Do("detp geom y2013_1x");
  return geom;
};