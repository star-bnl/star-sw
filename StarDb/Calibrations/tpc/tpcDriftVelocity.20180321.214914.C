TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80083
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55426; // +/- 1.12587e-05 cm/us All: East = -2.47296 +/- 0.00870578
  row.laserDriftVelocityWest	 =   5.55426; // +/- 1.12587e-05 cm/us All: West = -2.19584 +/- 0.0020804
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55426 +/- 1.12587e-05
  return (TDataSet *)tableSet;// West = 5.55418 +/- 1.15887e-05 East = 5.55574 +/- 4.75133e-05
};
