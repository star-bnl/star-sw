TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98059
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55069; // +/- 5.84184e-05 cm/us All: East = -0.0938162 +/- 0.247552
  row.laserDriftVelocityWest	 =   5.55069; // +/- 5.84184e-05 cm/us All: West = 0.328561 +/- 0.0468731
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55069 +/- 5.84184e-05
  return (TDataSet *)tableSet;// West = 5.54977 +/- 8.09192e-05 East = 5.55169 +/- 8.44244e-05
};
