TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54198; // +/- 1.09337e-05 cm/us All: East = 2.74527 +/- 50.6335
  row.laserDriftVelocityWest	 =   5.54198; // +/- 1.09337e-05 cm/us All: West = 1.99756 +/- 0.00195583
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54198 +/- 1.09337e-05
  return (TDataSet *)tableSet;// West = 5.54198 +/- 1.09337e-05 East = 5.53095 +/- 0.273549
};
