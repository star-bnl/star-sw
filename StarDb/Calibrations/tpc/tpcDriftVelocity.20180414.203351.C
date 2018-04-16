TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55352; // +/- 1.86257e-05 cm/us All: East = -5.68901 +/- 0.00492745
  row.laserDriftVelocityWest	 =   5.55352; // +/- 1.86257e-05 cm/us All: West = -5.41859 +/- 0.00505908
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55352 +/- 1.86257e-05
  return (TDataSet *)tableSet;// West = 5.55273 +/- 2.68046e-05 East = 5.55425 +/- 2.59e-05
};
