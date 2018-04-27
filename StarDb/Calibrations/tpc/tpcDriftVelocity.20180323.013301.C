TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53239; // +/- 1.46509e-05 cm/us All: East = 1.99747 +/- 0.0144698
  row.laserDriftVelocityWest	 =   5.53239; // +/- 1.46509e-05 cm/us All: West = 2.01732 +/- 0.00269419
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53239 +/- 1.46509e-05
  return (TDataSet *)tableSet;// West = 5.53239 +/- 1.49723e-05 East = 5.53256 +/- 7.10894e-05
};
