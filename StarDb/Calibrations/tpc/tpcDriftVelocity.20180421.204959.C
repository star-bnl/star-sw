TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54419; // +/- 1.90486e-05 cm/us All: East = -0.679603 +/- 0.0144198
  row.laserDriftVelocityWest	 =   5.54419; // +/- 1.90486e-05 cm/us All: West = 0.175198 +/- 0.00377513
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54419 +/- 1.90486e-05
  return (TDataSet *)tableSet;// West = 5.54386 +/- 1.97549e-05 East = 5.54846 +/- 7.18818e-05
};
