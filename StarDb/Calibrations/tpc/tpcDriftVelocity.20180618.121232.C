TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 169021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54612; // +/- 3.72768e-06 cm/us All: East = -0.247202 +/- 0.00192293
  row.laserDriftVelocityWest	 =   5.54612; // +/- 3.72768e-06 cm/us All: West = -0.220157 +/- 0.000706141
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54612 +/- 3.72768e-06
  return (TDataSet *)tableSet;// West = 5.54611 +/- 3.971e-06 East = 5.54626 +/- 1.08155e-05
};
