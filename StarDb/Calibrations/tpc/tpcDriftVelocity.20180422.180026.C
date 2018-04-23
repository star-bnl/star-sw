TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55262; // +/- 7.85065e-06 cm/us All: East = 0.0827678 +/- 0.00315529
  row.laserDriftVelocityWest	 =   5.55262; // +/- 7.85065e-06 cm/us All: West = 0.521222 +/- 0.00156213
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55262 +/- 7.85065e-06
  return (TDataSet *)tableSet;// West = 5.55215 +/- 8.81937e-06 East = 5.5544 +/- 1.72296e-05
};
