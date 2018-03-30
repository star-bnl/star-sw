TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5532; // +/- 1.13264e-05 cm/us All: East = -2.85455 +/- 0.0196639
  row.laserDriftVelocityWest	 =   5.5532; // +/- 1.13264e-05 cm/us All: West = -2.00914 +/- 0.00204412
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5532 +/- 1.13264e-05
  return (TDataSet *)tableSet;// West = 5.55313 +/- 1.1404e-05 East = 5.55796 +/- 9.72627e-05
};
