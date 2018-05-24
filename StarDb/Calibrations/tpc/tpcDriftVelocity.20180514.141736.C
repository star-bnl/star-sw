TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54802; // +/- 1.24549e-05 cm/us All: East = -0.451277 +/- 0.00332745
  row.laserDriftVelocityWest	 =   5.54802; // +/- 1.24549e-05 cm/us All: West = 0.190109 +/- 0.00300522
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54802 +/- 1.24549e-05
  return (TDataSet *)tableSet;// West = 5.54646 +/- 1.6612e-05 East = 5.55002 +/- 1.88221e-05
};
