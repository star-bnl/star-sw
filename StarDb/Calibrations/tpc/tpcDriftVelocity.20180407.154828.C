TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53571; // +/- 8.39278e-06 cm/us All: East = 0.0921037 +/- 0.00215053
  row.laserDriftVelocityWest	 =   5.53571; // +/- 8.39278e-06 cm/us All: West = 0.245504 +/- 0.00212712
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53571 +/- 8.39278e-06
  return (TDataSet *)tableSet;// West = 5.53528 +/- 1.1793e-05 East = 5.53615 +/- 1.19469e-05
};
