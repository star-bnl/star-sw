TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54287; // +/- 6.31705e-06 cm/us All: East = -0.0234201 +/- 0.0108495
  row.laserDriftVelocityWest	 =   5.54287; // +/- 6.31705e-06 cm/us All: West = 0.0237169 +/- 0.00113167
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54287 +/- 6.31705e-06
  return (TDataSet *)tableSet;// West = 5.54287 +/- 6.36703e-06 East = 5.54269 +/- 5.05139e-05
};
