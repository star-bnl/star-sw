TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54955; // +/- 2.2371e-05 cm/us All: East = -5.25709 +/- 0.0196754
  row.laserDriftVelocityWest	 =   5.54955; // +/- 2.2371e-05 cm/us All: West = -4.79492 +/- 0.00427309
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54955 +/- 2.2371e-05
  return (TDataSet *)tableSet;// West = 5.54921 +/- 2.42085e-05 East = 5.55149 +/- 5.85385e-05
};
