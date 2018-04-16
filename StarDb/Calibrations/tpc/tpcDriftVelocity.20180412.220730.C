TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55369; // +/- 1.22273e-05 cm/us All: East = -6.67244 +/- 0.00918915
  row.laserDriftVelocityWest	 =   5.55369; // +/- 1.22273e-05 cm/us All: West = -5.52802 +/- 0.00222223
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55369 +/- 1.22273e-05
  return (TDataSet *)tableSet;// West = 5.55332 +/- 1.25924e-05 East = 5.55974 +/- 5.11488e-05
};
