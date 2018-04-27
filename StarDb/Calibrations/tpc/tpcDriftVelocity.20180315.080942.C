TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52032; // +/- 0.000132386 cm/us All: East = 0.0161346 +/- 1.98722
  row.laserDriftVelocityWest	 =   5.52032; // +/- 0.000132386 cm/us All: West = 0.290519 +/- 0.0789324
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52032 +/- 0.000132386
  return (TDataSet *)tableSet;// West = 5.52031 +/- 0.000132593 East = 5.52331 +/- 0.00236587
};
