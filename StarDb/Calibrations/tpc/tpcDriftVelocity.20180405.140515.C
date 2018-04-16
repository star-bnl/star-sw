TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53272; // +/- 1.69016e-05 cm/us All: East = -2.53102 +/- 0.00691818
  row.laserDriftVelocityWest	 =   5.53272; // +/- 1.69016e-05 cm/us All: West = -1.66278 +/- 0.00335584
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53272 +/- 1.69016e-05
  return (TDataSet *)tableSet;// West = 5.53181 +/- 1.87767e-05 East = 5.53662 +/- 3.88005e-05
};
