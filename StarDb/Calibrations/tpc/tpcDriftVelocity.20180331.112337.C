TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51724; // +/- 6.97066e-05 cm/us All: East = -1.85555 +/- 23.3902
  row.laserDriftVelocityWest	 =   5.51724; // +/- 6.97066e-05 cm/us All: West = -0.0225203 +/- 0.045991
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51724 +/- 6.97066e-05
  return (TDataSet *)tableSet;// West = 5.51724 +/- 6.97066e-05 East = 5.59995 +/- 0.1351
};
