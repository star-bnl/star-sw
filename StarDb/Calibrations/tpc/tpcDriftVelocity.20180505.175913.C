TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 125032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55189; // +/- 7.93827e-06 cm/us All: East = 0.123623 +/- 0.0017996
  row.laserDriftVelocityWest	 =   5.55189; // +/- 7.93827e-06 cm/us All: West = 0.194595 +/- 0.00132021
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55189 +/- 7.93827e-06
  return (TDataSet *)tableSet;// West = 5.55117 +/- 1.2703e-05 East = 5.55235 +/- 1.01682e-05
};
