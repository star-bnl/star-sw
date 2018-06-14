TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54764; // +/- 1.13821e-05 cm/us All: East = -0.277809 +/- 0.00509275
  row.laserDriftVelocityWest	 =   5.54764; // +/- 1.13821e-05 cm/us All: West = 0.354616 +/- 0.00226037
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54764 +/- 1.13821e-05
  return (TDataSet *)tableSet;// West = 5.54708 +/- 1.24219e-05 East = 5.55057 +/- 2.84183e-05
};
