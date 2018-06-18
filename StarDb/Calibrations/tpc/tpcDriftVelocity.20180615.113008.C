TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 166015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54693; // +/- 8.84111e-06 cm/us All: East = -0.468394 +/- 0.00551316
  row.laserDriftVelocityWest	 =   5.54693; // +/- 8.84111e-06 cm/us All: West = 0.33561 +/- 0.00166038
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54693 +/- 8.84111e-06
  return (TDataSet *)tableSet;// West = 5.54651 +/- 9.28924e-06 East = 5.55099 +/- 2.88128e-05
};
