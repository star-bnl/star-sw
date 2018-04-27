TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 71037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54234; // +/- 3.65808e-05 cm/us All: East = 0.0600698 +/- 0.0169951
  row.laserDriftVelocityWest	 =   5.54234; // +/- 3.65808e-05 cm/us All: West = 0.333811 +/- 0.0106495
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54234 +/- 3.65808e-05
  return (TDataSet *)tableSet;// West = 5.54164 +/- 4.9227e-05 East = 5.54321 +/- 5.46653e-05
};
