TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 27001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53602; // +/- 8.78148e-06 cm/us All: East = 3.97411 +/- 0.00380298
  row.laserDriftVelocityWest	 =   5.53602; // +/- 8.78148e-06 cm/us All: West = 4.30475 +/- 0.00172608
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53602 +/- 8.78148e-06
  return (TDataSet *)tableSet;// West = 5.53578 +/- 9.66264e-06 East = 5.53713 +/- 2.10478e-05
};
