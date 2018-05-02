TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56559; // +/- 8.65539e-05 cm/us All: East = -3.81223 +/- 1.40423
  row.laserDriftVelocityWest	 =   5.56559; // +/- 8.65539e-05 cm/us All: West = -4.40682 +/- 0.158053
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56559 +/- 8.65539e-05
  return (TDataSet *)tableSet;// West = 5.56559 +/- 8.65602e-05 East = 5.57593 +/- 0.00718678
};
