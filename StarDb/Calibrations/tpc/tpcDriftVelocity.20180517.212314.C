TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53938; // +/- 4.94423e-06 cm/us All: East = -0.179263 +/- 0.00274156
  row.laserDriftVelocityWest	 =   5.53938; // +/- 4.94423e-06 cm/us All: West = -0.154569 +/- 0.000918027
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53938 +/- 4.94423e-06
  return (TDataSet *)tableSet;// West = 5.53937 +/- 5.21417e-06 East = 5.53948 +/- 1.55682e-05
};
