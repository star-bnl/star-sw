TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 68054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56697; // +/- 4.40952e-05 cm/us All: East = -0.48281 +/- 0.0370581
  row.laserDriftVelocityWest	 =   5.56697; // +/- 4.40952e-05 cm/us All: West = 0.318069 +/- 0.0172906
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56697 +/- 4.40952e-05
  return (TDataSet *)tableSet;// West = 5.56564 +/- 5.23658e-05 East = 5.57022 +/- 8.17513e-05
};
