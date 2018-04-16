TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5463; // +/- 9.27952e-06 cm/us All: East = -4.31369 +/- 0.00240528
  row.laserDriftVelocityWest	 =   5.5463; // +/- 9.27952e-06 cm/us All: West = -4.22695 +/- 0.00227799
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5463 +/- 9.27952e-06
  return (TDataSet *)tableSet;// West = 5.54607 +/- 1.28176e-05 East = 5.54655 +/- 1.34518e-05
};
