TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 126052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56936; // +/- 8.7421e-06 cm/us All: East = -0.782042 +/- 0.00287346
  row.laserDriftVelocityWest	 =   5.56936; // +/- 8.7421e-06 cm/us All: West = -0.174275 +/- 0.00188271
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56936 +/- 8.7421e-06
  return (TDataSet *)tableSet;// West = 5.56836 +/- 1.04722e-05 East = 5.57167 +/- 1.58783e-05
};
