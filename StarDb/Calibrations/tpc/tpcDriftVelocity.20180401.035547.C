TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52818; // +/- 1.15021e-05 cm/us All: East = -0.553202 +/- 0.0101564
  row.laserDriftVelocityWest	 =   5.52818; // +/- 1.15021e-05 cm/us All: West = 0.236254 +/- 0.00208848
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52818 +/- 1.15021e-05
  return (TDataSet *)tableSet;// West = 5.52803 +/- 1.17098e-05 East = 5.5323 +/- 6.13431e-05
};
