TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54079; // +/- 5.70447e-06 cm/us All: East = -0.160763 +/- 0.00333796
  row.laserDriftVelocityWest	 =   5.54079; // +/- 5.70447e-06 cm/us All: West = 0.131897 +/- 0.00106092
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54079 +/- 5.70447e-06
  return (TDataSet *)tableSet;// West = 5.54068 +/- 5.94678e-06 East = 5.54202 +/- 2.01896e-05
};
