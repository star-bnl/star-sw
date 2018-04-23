TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55408; // +/- 8.56005e-06 cm/us All: East = 0.091268 +/- 0.0018954
  row.laserDriftVelocityWest	 =   5.55408; // +/- 8.56005e-06 cm/us All: West = 0.358399 +/- 0.00258068
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55408 +/- 8.56005e-06
  return (TDataSet *)tableSet;// West = 5.55312 +/- 1.4373e-05 East = 5.55461 +/- 1.0656e-05
};
