TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55159; // +/- 0.000108096 cm/us All: East = -3.20859 +/- 3.56011
  row.laserDriftVelocityWest	 =   5.55159; // +/- 0.000108096 cm/us All: West = 0.169419 +/- 0.0267836
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55159 +/- 0.000108096
  return (TDataSet *)tableSet;// West = 5.54752 +/- 0.000242572 East = 5.5526 +/- 0.000120748
};
