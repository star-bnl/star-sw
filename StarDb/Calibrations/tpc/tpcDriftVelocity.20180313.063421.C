TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53228; // +/- 1.5075e-05 cm/us All: East = 1.8549 +/- 0.00941781
  row.laserDriftVelocityWest	 =   5.53228; // +/- 1.5075e-05 cm/us All: West = 2.00553 +/- 0.00282909
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53228 +/- 1.5075e-05
  return (TDataSet *)tableSet;// West = 5.53219 +/- 1.58406e-05 East = 5.5331 +/- 4.90829e-05
};
