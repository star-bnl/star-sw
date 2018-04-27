TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 115002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54216; // +/- 6.23356e-06 cm/us All: East = -0.492136 +/- 0.00571718
  row.laserDriftVelocityWest	 =   5.54216; // +/- 6.23356e-06 cm/us All: West = 0.173496 +/- 0.00113265
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54216 +/- 6.23356e-06
  return (TDataSet *)tableSet;// West = 5.54202 +/- 6.35683e-06 East = 5.54564 +/- 3.18079e-05
};
