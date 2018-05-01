TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 119024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5366; // +/- 2.58442e-05 cm/us All: East = -0.769343 +/- 0.00908677
  row.laserDriftVelocityWest	 =   5.5366; // +/- 2.58442e-05 cm/us All: West = 0.30188 +/- 0.00574329
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5366 +/- 2.58442e-05
  return (TDataSet *)tableSet;// West = 5.53493 +/- 3.05537e-05 East = 5.54077 +/- 4.84511e-05
};
