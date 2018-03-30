TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 71037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54264; // +/- 3.22839e-05 cm/us All: East = -0.250217 +/- 0.00999849
  row.laserDriftVelocityWest	 =   5.54264; // +/- 3.22839e-05 cm/us All: West = 0.00594502 +/- 0.0106369
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54264 +/- 3.22839e-05
  return (TDataSet *)tableSet;// West = 5.54191 +/- 4.73581e-05 East = 5.54327 +/- 4.41257e-05
};
