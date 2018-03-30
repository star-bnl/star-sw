TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74101
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53206; // +/- 1.0256e-05 cm/us All: East = 1.5954 +/- 0.00870731
  row.laserDriftVelocityWest	 =   5.53206; // +/- 1.0256e-05 cm/us All: West = 1.79651 +/- 0.00185889
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53206 +/- 1.0256e-05
  return (TDataSet *)tableSet;// West = 5.53206 +/- 1.0256e-05 East = 5.48921 +/- 1.41421
};
