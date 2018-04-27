TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55488; // +/- 6.84381e-06 cm/us All: East = 2.03476 +/- 0.00508043
  row.laserDriftVelocityWest	 =   5.55488; // +/- 6.84381e-06 cm/us All: West = 2.00281 +/- 0.00126444
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55488 +/- 6.84381e-06
  return (TDataSet *)tableSet;// West = 5.55489 +/- 7.0545e-06 East = 5.55483 +/- 2.82138e-05
};
