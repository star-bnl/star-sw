TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52387; // +/- 9.03132e-06 cm/us All: East = 1.04325 +/- 8.01341
  row.laserDriftVelocityWest	 =   5.52387; // +/- 9.03132e-06 cm/us All: West = 2.03303 +/- 0.00161757
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52387 +/- 9.03132e-06
  return (TDataSet *)tableSet;// West = 5.52387 +/- 9.03133e-06 East = 5.52119 +/- 0.0234297
};
