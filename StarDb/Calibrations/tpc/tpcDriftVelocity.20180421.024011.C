TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54352; // +/- 6.09557e-06 cm/us All: East = -0.499126 +/- 0.00525864
  row.laserDriftVelocityWest	 =   5.54352; // +/- 6.09557e-06 cm/us All: West = 0.166887 +/- 0.00110681
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54352 +/- 6.09557e-06
  return (TDataSet *)tableSet;// West = 5.54336 +/- 6.23766e-06 East = 5.54695 +/- 2.87225e-05
};
