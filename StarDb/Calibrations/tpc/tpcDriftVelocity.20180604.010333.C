TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55353; // +/- 6.02315e-06 cm/us All: East = 0.179392 +/- 0.00341536
  row.laserDriftVelocityWest	 =   5.55353; // +/- 6.02315e-06 cm/us All: West = 0.0600894 +/- 0.00112914
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55353 +/- 6.02315e-06
  return (TDataSet *)tableSet;// West = 5.5536 +/- 6.35822e-06 East = 5.5529 +/- 1.88021e-05
};
