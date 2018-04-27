TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54475; // +/- 3.1078e-05 cm/us All: East = -0.222709 +/- 0.00568866
  row.laserDriftVelocityWest	 =   5.54475; // +/- 3.1078e-05 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54475 +/- 3.1078e-05
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.54475 +/- 3.1078e-05
};
