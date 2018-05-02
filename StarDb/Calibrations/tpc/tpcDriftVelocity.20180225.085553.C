TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 56008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50748; // +/- 1.29914e-05 cm/us All: East = -0.147455 +/- 0.00555848
  row.laserDriftVelocityWest	 =   5.50748; // +/- 1.29914e-05 cm/us All: West = 0.251542 +/- 0.00267231
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50748 +/- 1.29914e-05
  return (TDataSet *)tableSet;// West = 5.50711 +/- 1.44998e-05 East = 5.50901 +/- 2.92526e-05
};
