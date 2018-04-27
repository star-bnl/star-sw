TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54332; // +/- 0.00041262 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54332; // +/- 0.00041262 cm/us All: West = 1.97916 +/- 0.363743
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54332 +/- 0.00041262
  return (TDataSet *)tableSet;// West = 5.54332 +/- 0.00041262 East = -999 +/- 999
};
