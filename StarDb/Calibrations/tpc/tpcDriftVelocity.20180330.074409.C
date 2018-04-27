TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51658; // +/- 4.93704e-05 cm/us All: East = 0.0641155 +/- 24.6187
  row.laserDriftVelocityWest	 =   5.51658; // +/- 4.93704e-05 cm/us All: West = 0.168815 +/- 0.0145494
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51658 +/- 4.93704e-05
  return (TDataSet *)tableSet;// West = 5.51658 +/- 4.93706e-05 East = 5.51777 +/- 0.0165459
};
