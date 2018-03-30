TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5559; // +/- 0.000188381 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.5559; // +/- 0.000188381 cm/us All: West = -2.44038 +/- 0.355613
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5559 +/- 0.000188381
  return (TDataSet *)tableSet;// West = 5.5559 +/- 0.000188381 East = -999 +/- 999
};
