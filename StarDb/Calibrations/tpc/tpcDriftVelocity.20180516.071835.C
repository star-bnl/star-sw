TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53785; // +/- 5.29824e-06 cm/us All: East = 0.108088 +/- 0.00256483
  row.laserDriftVelocityWest	 =   5.53785; // +/- 5.29824e-06 cm/us All: West = 0.179296 +/- 0.00101211
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53785 +/- 5.29824e-06
  return (TDataSet *)tableSet;// West = 5.53779 +/- 5.68306e-06 East = 5.53823 +/- 1.46474e-05
};
