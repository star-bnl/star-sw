TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 101011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54194; // +/- 1.05151e-05 cm/us All: East = -0.348299 +/- 0.0145517
  row.laserDriftVelocityWest	 =   5.54194; // +/- 1.05151e-05 cm/us All: West = 0.165635 +/- 0.00190254
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54194 +/- 1.05151e-05
  return (TDataSet *)tableSet;// West = 5.54185 +/- 1.068e-05 East = 5.54484 +/- 6.00744e-05
};
