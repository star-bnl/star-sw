TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 55017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50612; // +/- 1.94398e-05 cm/us All: East = -0.171648 +/- 0.00628855
  row.laserDriftVelocityWest	 =   5.50612; // +/- 1.94398e-05 cm/us All: West = 0.258927 +/- 0.00485727
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50612 +/- 1.94398e-05
  return (TDataSet *)tableSet;// West = 5.50545 +/- 2.63066e-05 East = 5.50692 +/- 2.88537e-05
};
