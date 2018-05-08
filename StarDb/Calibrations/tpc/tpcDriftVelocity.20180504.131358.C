TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52743; // +/- 1.35357e-05 cm/us All: East = -0.205804 +/- 0.00325671
  row.laserDriftVelocityWest	 =   5.52743; // +/- 1.35357e-05 cm/us All: West = 0.741341 +/- 0.00362756
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52743 +/- 1.35357e-05
  return (TDataSet *)tableSet;// West = 5.52464 +/- 1.99155e-05 East = 5.52982 +/- 1.84528e-05
};
