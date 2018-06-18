TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54575; // +/- 6.00894e-06 cm/us All: East = 0.323953 +/- 0.00198925
  row.laserDriftVelocityWest	 =   5.54575; // +/- 6.00894e-06 cm/us All: West = 0.200904 +/- 0.0012789
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54575 +/- 6.00894e-06
  return (TDataSet *)tableSet;// West = 5.54595 +/- 7.1679e-06 East = 5.54528 +/- 1.10217e-05
};
