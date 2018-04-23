TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53711; // +/- 1.09133e-05 cm/us All: East = -0.538444 +/- 0.00828005
  row.laserDriftVelocityWest	 =   5.53711; // +/- 1.09133e-05 cm/us All: West = 0.243979 +/- 0.00199581
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53711 +/- 1.09133e-05
  return (TDataSet *)tableSet;// West = 5.53689 +/- 1.12261e-05 East = 5.54096 +/- 4.65556e-05
};
