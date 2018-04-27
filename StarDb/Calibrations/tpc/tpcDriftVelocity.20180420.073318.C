TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54369; // +/- 5.19244e-06 cm/us All: East = 0.0142325 +/- 0.00210506
  row.laserDriftVelocityWest	 =   5.54369; // +/- 5.19244e-06 cm/us All: West = 0.215282 +/- 0.00102265
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54369 +/- 5.19244e-06
  return (TDataSet *)tableSet;// West = 5.54347 +/- 5.78603e-06 East = 5.54458 +/- 1.1769e-05
};
