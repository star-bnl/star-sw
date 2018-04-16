TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55401; // +/- 2.08031e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.55401; // +/- 2.08031e-05 cm/us All: West = -5.64814 +/- 0.00366956
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55401 +/- 2.08031e-05
  return (TDataSet *)tableSet;// West = 5.55401 +/- 2.08031e-05 East = -999 +/- 999
};
