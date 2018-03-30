TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74111
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53443; // +/- 1.08196e-05 cm/us All: East = 1.20608 +/- 0.0111691
  row.laserDriftVelocityWest	 =   5.53443; // +/- 1.08196e-05 cm/us All: West = 1.3657 +/- 0.00198575
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53443 +/- 1.08196e-05
  return (TDataSet *)tableSet;// West = 5.5344 +/- 1.10069e-05 East = 5.53535 +/- 5.89028e-05
};
