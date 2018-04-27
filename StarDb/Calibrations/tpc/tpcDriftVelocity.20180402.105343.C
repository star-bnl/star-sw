TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51806; // +/- 1.89737e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.51806; // +/- 1.89737e-05 cm/us All: West = 0.0732069 +/- 0.00350239
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51806 +/- 1.89737e-05
  return (TDataSet *)tableSet;// West = 5.51806 +/- 1.89737e-05 East = -999 +/- 999
};
