TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54265; // +/- 9.38149e-06 cm/us All: East = -0.037853 +/- 0.00266709
  row.laserDriftVelocityWest	 =   5.54265; // +/- 9.38149e-06 cm/us All: West = 0.267783 +/- 0.0014341
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54265 +/- 9.38149e-06
  return (TDataSet *)tableSet;// West = 5.54175 +/- 1.21098e-05 East = 5.54401 +/- 1.48365e-05
};
