TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53185; // +/- 1.05165e-05 cm/us All: East = -0.27489 +/- 0.00728613
  row.laserDriftVelocityWest	 =   5.53185; // +/- 1.05165e-05 cm/us All: West = 0.233513 +/- 0.00196131
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53185 +/- 1.05165e-05
  return (TDataSet *)tableSet;// West = 5.53164 +/- 1.09407e-05 East = 5.53446 +/- 3.81399e-05
};
