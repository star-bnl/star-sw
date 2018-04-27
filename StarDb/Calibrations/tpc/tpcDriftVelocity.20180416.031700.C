TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5445; // +/- 8.83324e-06 cm/us All: East = 0.00717365 +/- 0.00210239
  row.laserDriftVelocityWest	 =   5.5445; // +/- 8.83324e-06 cm/us All: West = 0.222218 +/- 0.00239476
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5445 +/- 8.83324e-06
  return (TDataSet *)tableSet;// West = 5.54386 +/- 1.32626e-05 East = 5.54501 +/- 1.1842e-05
};
