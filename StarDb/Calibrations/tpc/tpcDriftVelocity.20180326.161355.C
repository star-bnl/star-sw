TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52877; // +/- 1.74726e-05 cm/us All: East = 2.0861 +/- 0.00649704
  row.laserDriftVelocityWest	 =   5.52877; // +/- 1.74726e-05 cm/us All: West = 2.4587 +/- 0.00374458
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52877 +/- 1.74726e-05
  return (TDataSet *)tableSet;// West = 5.52838 +/- 2.01234e-05 East = 5.52998 +/- 3.52206e-05
};
