TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55287; // +/- 7.13903e-06 cm/us All: East = 0.0735789 +/- 0.00272075
  row.laserDriftVelocityWest	 =   5.55287; // +/- 7.13903e-06 cm/us All: West = 0.47804 +/- 0.00144022
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55287 +/- 7.13903e-06
  return (TDataSet *)tableSet;// West = 5.5524 +/- 8.11106e-06 East = 5.5545 +/- 1.50398e-05
};
