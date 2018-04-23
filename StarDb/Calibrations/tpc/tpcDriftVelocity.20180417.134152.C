TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55552; // +/- 1.33902e-05 cm/us All: East = 0.0344704 +/- 0.0031625
  row.laserDriftVelocityWest	 =   5.55552; // +/- 1.33902e-05 cm/us All: West = 0.380907 +/- 0.00379611
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55552 +/- 1.33902e-05
  return (TDataSet *)tableSet;// West = 5.55439 +/- 2.07976e-05 East = 5.55632 +/- 1.74997e-05
};
