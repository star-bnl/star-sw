TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54422; // +/- 1.25894e-05 cm/us All: East = -3.92266 +/- 0.0022276
  row.laserDriftVelocityWest	 =   5.54422; // +/- 1.25894e-05 cm/us All: West = -2.83436 +/- 1.16338
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54422 +/- 1.25894e-05
  return (TDataSet *)tableSet;// West = 5.53788 +/- 0.000542872 East = 5.54422 +/- 1.25927e-05
};
