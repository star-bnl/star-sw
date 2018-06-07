TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54998; // +/- 4.81797e-06 cm/us All: East = 0.127622 +/- 0.00170241
  row.laserDriftVelocityWest	 =   5.54998; // +/- 4.81797e-06 cm/us All: West = 0.0161512 +/- 0.000982082
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54998 +/- 4.81797e-06
  return (TDataSet *)tableSet;// West = 5.55013 +/- 5.58781e-06 East = 5.54953 +/- 9.51193e-06
};
