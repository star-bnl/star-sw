TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55662; // +/- 1.72037e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.55662; // +/- 1.72037e-05 cm/us All: West = -2.63921 +/- 0.00307004
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55662 +/- 1.72037e-05
  return (TDataSet *)tableSet;// West = 5.55662 +/- 1.72037e-05 East = -999 +/- 999
};
