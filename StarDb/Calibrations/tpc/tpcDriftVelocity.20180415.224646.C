TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55402; // +/- 8.85106e-06 cm/us All: East = -5.75154 +/- 0.00207431
  row.laserDriftVelocityWest	 =   5.55402; // +/- 8.85106e-06 cm/us All: West = -5.51848 +/- 0.0024773
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55402 +/- 8.85106e-06
  return (TDataSet *)tableSet;// West = 5.55326 +/- 1.37341e-05 East = 5.55456 +/- 1.15755e-05
};
