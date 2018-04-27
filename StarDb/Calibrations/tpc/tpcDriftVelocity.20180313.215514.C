TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53685; // +/- 0.000259341 cm/us All: East = -0.20428 +/- 0.00974766
  row.laserDriftVelocityWest	 =   5.53685; // +/- 0.000259341 cm/us All: West = 0.348813 +/- 0.00424061
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53685 +/- 0.000259341
  return (TDataSet *)tableSet;// West = 5.62672 +/- 0.00223252 East = 5.53562 +/- 0.000261109
};
