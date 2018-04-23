TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5442; // +/- 1.21416e-05 cm/us All: East = 0.184089 +/- 0.0022143
  row.laserDriftVelocityWest	 =   5.5442; // +/- 1.21416e-05 cm/us All: West = 1.53063 +/- 1.91283
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5442 +/- 1.21416e-05
  return (TDataSet *)tableSet;// West = 5.53793 +/- 0.000657358 East = 5.5442 +/- 1.21437e-05
};
