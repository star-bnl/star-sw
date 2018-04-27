TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51927; // +/- 7.71135e-06 cm/us All: East = -0.750144 +/- 0.00601774
  row.laserDriftVelocityWest	 =   5.51927; // +/- 7.71135e-06 cm/us All: West = -0.00237548 +/- 0.00144393
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51927 +/- 7.71135e-06
  return (TDataSet *)tableSet;// West = 5.51908 +/- 7.90717e-06 East = 5.52298 +/- 3.48658e-05
};
