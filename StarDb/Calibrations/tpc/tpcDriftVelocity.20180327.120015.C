TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52911; // +/- 7.8967e-06 cm/us All: East = 1.62841 +/- 0.00562304
  row.laserDriftVelocityWest	 =   5.52911; // +/- 7.8967e-06 cm/us All: West = 2.37439 +/- 0.00146384
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52911 +/- 7.8967e-06
  return (TDataSet *)tableSet;// West = 5.52886 +/- 8.1492e-06 East = 5.53294 +/- 3.19702e-05
};
