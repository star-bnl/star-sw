TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56386; // +/- 1.03423e-05 cm/us All: East = 0.560201 +/- 0.0134232
  row.laserDriftVelocityWest	 =   5.56386; // +/- 1.03423e-05 cm/us All: West = 0.17319 +/- 0.00185149
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56386 +/- 1.03423e-05
  return (TDataSet *)tableSet;// West = 5.56391 +/- 1.04651e-05 East = 5.56166 +/- 6.76894e-05
};
