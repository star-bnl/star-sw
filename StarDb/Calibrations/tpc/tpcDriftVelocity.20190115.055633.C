TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 15002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55362; // +/- 1.94591e-05 cm/us All: East = -0.233956 +/- 0.0103618
  row.laserDriftVelocityWest	 =   5.55362; // +/- 1.94591e-05 cm/us All: West = 0.0106994 +/- 0.00378291
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55362 +/- 1.94591e-05
  return (TDataSet *)tableSet;// West = 5.55349 +/- 2.06213e-05 East = 5.55468 +/- 5.87951e-05
};
