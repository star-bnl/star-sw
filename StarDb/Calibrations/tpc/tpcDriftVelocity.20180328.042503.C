TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51826; // +/- 8.35209e-06 cm/us All: East = -0.714508 +/- 0.00955578
  row.laserDriftVelocityWest	 =   5.51826; // +/- 8.35209e-06 cm/us All: West = 0.206795 +/- 0.00151403
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51826 +/- 8.35209e-06
  return (TDataSet *)tableSet;// West = 5.51814 +/- 8.45406e-06 East = 5.52318 +/- 5.39371e-05
};
