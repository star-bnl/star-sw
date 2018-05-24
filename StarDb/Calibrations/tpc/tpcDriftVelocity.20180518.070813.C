TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54255; // +/- 1.04966e-05 cm/us All: East = -0.20509 +/- 0.00788317
  row.laserDriftVelocityWest	 =   5.54255; // +/- 1.04966e-05 cm/us All: West = -0.113357 +/- 0.00103137
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54255 +/- 1.04966e-05
  return (TDataSet *)tableSet;// West = 5.54245 +/- 1.08668e-05 East = 5.54408 +/- 4.05607e-05
};
