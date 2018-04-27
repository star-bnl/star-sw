TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52088; // +/- 8.72801e-06 cm/us All: East = -0.304412 +/- 0.00901785
  row.laserDriftVelocityWest	 =   5.52088; // +/- 8.72801e-06 cm/us All: West = 0.187103 +/- 0.00159304
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52088 +/- 8.72801e-06
  return (TDataSet *)tableSet;// West = 5.52079 +/- 8.86572e-06 East = 5.52359 +/- 4.97131e-05
};
