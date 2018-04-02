TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91119
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.529; // +/- 1.40289e-05 cm/us All: East = -0.67128 +/- 0.00648561
  row.laserDriftVelocityWest	 =   5.529; // +/- 1.40289e-05 cm/us All: West = 0.25651 +/- 0.00276848
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.529 +/- 1.40289e-05
  return (TDataSet *)tableSet;// West = 5.52821 +/- 1.52725e-05 East = 5.53332 +/- 3.5494e-05
};
