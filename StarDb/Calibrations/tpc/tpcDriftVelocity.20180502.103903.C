TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 122014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53153; // +/- 9.19015e-06 cm/us All: East = -0.76915 +/- 0.00509454
  row.laserDriftVelocityWest	 =   5.53153; // +/- 9.19015e-06 cm/us All: West = 0.239137 +/- 0.00172278
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53153 +/- 9.19015e-06
  return (TDataSet *)tableSet;// West = 5.53098 +/- 9.6756e-06 East = 5.53655 +/- 2.93825e-05
};
