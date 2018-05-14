TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54815; // +/- 9.6039e-06 cm/us All: East = -0.161494 +/- 0.00393294
  row.laserDriftVelocityWest	 =   5.54815; // +/- 9.6039e-06 cm/us All: West = 0.24386 +/- 0.00131943
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54815 +/- 9.6039e-06
  return (TDataSet *)tableSet;// West = 5.54755 +/- 1.08852e-05 East = 5.55026 +/- 2.04031e-05
};
