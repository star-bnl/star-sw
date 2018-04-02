TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91095
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52839; // +/- 1.38961e-05 cm/us All: East = -0.592081 +/- 0.0092531
  row.laserDriftVelocityWest	 =   5.52839; // +/- 1.38961e-05 cm/us All: West = 0.289612 +/- 0.00259763
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52839 +/- 1.38961e-05
  return (TDataSet *)tableSet;// West = 5.52803 +/- 1.44456e-05 East = 5.53287 +/- 5.0864e-05
};
