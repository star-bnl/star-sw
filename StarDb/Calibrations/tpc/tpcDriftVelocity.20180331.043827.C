TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51709; // +/- 1.1164e-05 cm/us All: East = -0.574272 +/- 0.00959087
  row.laserDriftVelocityWest	 =   5.51709; // +/- 1.1164e-05 cm/us All: West = 0.308387 +/- 0.00206119
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51709 +/- 1.1164e-05
  return (TDataSet *)tableSet;// West = 5.51689 +/- 1.13997e-05 East = 5.5217 +/- 5.5184e-05
};
