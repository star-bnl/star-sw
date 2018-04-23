TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55382; // +/- 1.89122e-05 cm/us All: East = -0.390195 +/- 0.736203
  row.laserDriftVelocityWest	 =   5.55382; // +/- 1.89122e-05 cm/us All: West = 0.205705 +/- 0.00338008
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55382 +/- 1.89122e-05
  return (TDataSet *)tableSet;// West = 5.55382 +/- 1.89144e-05 East = 5.55442 +/- 0.00124908
};
