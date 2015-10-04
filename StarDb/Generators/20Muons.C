TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  St_geant_Maker *geantMk = StMaker::GetChain()->GetMaker("geant");
  // gkine #particles partid ptrange yrange phirange vertexrange
  geantMk->Do("gkine        20      6    1. 1. -1. 1. 0 6.28      0. 0.;");
  geantMk->Do("gspread   0.015 0.015 42.00");
  TDataSet *tableSet = new TDataSet("Pythia");
  return (TDataSet *)tableSet;
 }
