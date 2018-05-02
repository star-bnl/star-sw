TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120068
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53395; // +/- 6.8546e-06 cm/us All: East = -0.186543 +/- 0.00303473
  row.laserDriftVelocityWest	 =   5.53395; // +/- 6.8546e-06 cm/us All: West = 0.278104 +/- 0.00134092
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53395 +/- 6.8546e-06
  return (TDataSet *)tableSet;// West = 5.53353 +/- 7.5246e-06 East = 5.53597 +/- 1.66173e-05
};
