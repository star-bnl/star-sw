TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 66033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53078; // +/- 1.23819e-05 cm/us All: East = -0.34529 +/- 0.00419038
  row.laserDriftVelocityWest	 =   5.53078; // +/- 1.23819e-05 cm/us All: West = 0.365581 +/- 0.002622
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53078 +/- 1.23819e-05
  return (TDataSet *)tableSet;// West = 5.52975 +/- 1.46029e-05 East = 5.53344 +/- 2.33553e-05
};
