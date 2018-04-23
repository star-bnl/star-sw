TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74111
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53457; // +/- 1.06383e-05 cm/us All: East = -0.0971728 +/- 0.00971185
  row.laserDriftVelocityWest	 =   5.53457; // +/- 1.06383e-05 cm/us All: West = 0.175439 +/- 0.00193098
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53457 +/- 1.06383e-05
  return (TDataSet *)tableSet;// West = 5.53451 +/- 1.08346e-05 East = 5.53619 +/- 5.61397e-05
};
