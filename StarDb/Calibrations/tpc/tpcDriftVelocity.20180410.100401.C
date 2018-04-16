TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55389; // +/- 2.31218e-05 cm/us All: East = -5.6977 +/- 0.00441905
  row.laserDriftVelocityWest	 =   5.55389; // +/- 2.31218e-05 cm/us All: West = -4.5249 +/- 0.110204
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55389 +/- 2.31218e-05
  return (TDataSet *)tableSet;// West = 5.54854 +/- 0.000139444 East = 5.55404 +/- 2.34463e-05
};
