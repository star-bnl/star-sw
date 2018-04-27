TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54482; // +/- 1.54176e-05 cm/us All: East = -0.0606985 +/- 4.05608
  row.laserDriftVelocityWest	 =   5.54482; // +/- 1.54176e-05 cm/us All: West = 0.142902 +/- 0.00277484
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54482 +/- 1.54176e-05
  return (TDataSet *)tableSet;// West = 5.54482 +/- 1.54178e-05 East = 5.5467 +/- 0.00321429
};
