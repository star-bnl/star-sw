TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148022
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55094; // +/- 5.08075e-06 cm/us All: East = 0.219525 +/- 0.00241864
  row.laserDriftVelocityWest	 =   5.55094; // +/- 5.08075e-06 cm/us All: West = 0.156095 +/- 0.000967929
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55094 +/- 5.08075e-06
  return (TDataSet *)tableSet;// West = 5.55098 +/- 5.46808e-06 East = 5.55067 +/- 1.37442e-05
};
