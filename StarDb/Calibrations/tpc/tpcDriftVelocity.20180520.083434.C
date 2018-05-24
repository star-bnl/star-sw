TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 140011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55873; // +/- 5.78869e-06 cm/us All: East = -0.00513506 +/- 0.00344051
  row.laserDriftVelocityWest	 =   5.55873; // +/- 5.78869e-06 cm/us All: West = 0.229034 +/- 0.00107523
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55873 +/- 5.78869e-06
  return (TDataSet *)tableSet;// West = 5.55863 +/- 6.06487e-06 East = 5.55977 +/- 1.94037e-05
};
