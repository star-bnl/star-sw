TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54926; // +/- 1.51475e-05 cm/us All: East = 0.0809535 +/- 0.00800494
  row.laserDriftVelocityWest	 =   5.54926; // +/- 1.51475e-05 cm/us All: West = 0.100311 +/- 0.00326247
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54926 +/- 1.51475e-05
  return (TDataSet *)tableSet;// West = 5.54922 +/- 1.71228e-05 East = 5.54937 +/- 3.24863e-05
};
