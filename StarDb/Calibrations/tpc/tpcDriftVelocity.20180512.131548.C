TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54788; // +/- 7.53407e-06 cm/us All: East = -0.147829 +/- 0.0034345
  row.laserDriftVelocityWest	 =   5.54788; // +/- 7.53407e-06 cm/us All: West = 0.143352 +/- 0.00152163
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54788 +/- 7.53407e-06
  return (TDataSet *)tableSet;// West = 5.54753 +/- 8.37231e-06 East = 5.54937 +/- 1.72745e-05
};
