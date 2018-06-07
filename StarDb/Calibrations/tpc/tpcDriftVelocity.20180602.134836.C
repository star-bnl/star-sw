TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55205; // +/- 5.25057e-06 cm/us All: East = 0.312428 +/- 0.00233023
  row.laserDriftVelocityWest	 =   5.55205; // +/- 5.25057e-06 cm/us All: West = 0.119252 +/- 0.00100788
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55205 +/- 5.25057e-06
  return (TDataSet *)tableSet;// West = 5.55222 +/- 5.75078e-06 East = 5.55115 +/- 1.28715e-05
};
