TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5396; // +/- 1.18583e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.5396; // +/- 1.18583e-05 cm/us All: West = 2.07469 +/- 0.00213316
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5396 +/- 1.18583e-05
  return (TDataSet *)tableSet;// West = 5.5396 +/- 1.18583e-05 East = -999 +/- 999
};
