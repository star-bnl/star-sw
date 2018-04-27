TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54482; // +/- 3.05893e-05 cm/us All: East = -0.194279 +/- 0.00563587
  row.laserDriftVelocityWest	 =   5.54482; // +/- 3.05893e-05 cm/us All: West = 1.17463 +/- 0.128033
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54482 +/- 3.05893e-05
  return (TDataSet *)tableSet;// West = 5.53753 +/- 0.000220923 East = 5.54496 +/- 3.08868e-05
};
