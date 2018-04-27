TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51886; // +/- 1.06427e-05 cm/us All: East = -0.428375 +/- 0.0058179
  row.laserDriftVelocityWest	 =   5.51886; // +/- 1.06427e-05 cm/us All: West = 0.298806 +/- 0.00203179
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51886 +/- 1.06427e-05
  return (TDataSet *)tableSet;// West = 5.51843 +/- 1.126e-05 East = 5.52248 +/- 3.25914e-05
};
