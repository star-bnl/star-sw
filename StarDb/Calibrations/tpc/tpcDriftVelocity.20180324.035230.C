TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.525; // +/- 9.32948e-06 cm/us All: East = 1.31449 +/- 0.00578743
  row.laserDriftVelocityWest	 =   5.525; // +/- 9.32948e-06 cm/us All: West = 2.04365 +/- 0.00175392
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.525 +/- 9.32948e-06
  return (TDataSet *)tableSet;// West = 5.52467 +/- 9.72959e-06 East = 5.52868 +/- 3.2871e-05
};
