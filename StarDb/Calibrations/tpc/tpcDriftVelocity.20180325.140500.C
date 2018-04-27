TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52251; // +/- 9.16175e-06 cm/us All: East = 1.60119 +/- 0.005197
  row.laserDriftVelocityWest	 =   5.52251; // +/- 9.16175e-06 cm/us All: West = 2.04387 +/- 0.00174252
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52251 +/- 9.16175e-06
  return (TDataSet *)tableSet;// West = 5.52226 +/- 9.68504e-06 East = 5.52463 +/- 2.82548e-05
};
