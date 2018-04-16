TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5541; // +/- 7.65633e-06 cm/us All: East = -5.76207 +/- 0.00179346
  row.laserDriftVelocityWest	 =   5.5541; // +/- 7.65633e-06 cm/us All: West = -5.53277 +/- 0.00210388
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5541 +/- 7.65633e-06
  return (TDataSet *)tableSet;// West = 5.55335 +/- 1.17996e-05 East = 5.55465 +/- 1.00622e-05
};
