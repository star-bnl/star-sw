TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55428; // +/- 0.000102397 cm/us All: East = -5.88031 +/- 0.0435284
  row.laserDriftVelocityWest	 =   5.55428; // +/- 0.000102397 cm/us All: West = -3.79808 +/- 4.38692
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55428 +/- 0.000102397
  return (TDataSet *)tableSet;// West = 5.5452 +/- 0.000354016 East = 5.55511 +/- 0.000106969
};
