TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54871; // +/- 6.40833e-06 cm/us All: East = 0.103164 +/- 0.00223501
  row.laserDriftVelocityWest	 =   5.54871; // +/- 6.40833e-06 cm/us All: West = 0.216591 +/- 0.00131596
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54871 +/- 6.40833e-06
  return (TDataSet *)tableSet;// West = 5.54859 +/- 7.38122e-06 East = 5.5491 +/- 1.29141e-05
};
