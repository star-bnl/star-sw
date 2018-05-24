TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54518; // +/- 6.26359e-06 cm/us All: East = 0.222687 +/- 0.00345227
  row.laserDriftVelocityWest	 =   5.54518; // +/- 6.26359e-06 cm/us All: West = 0.0352233 +/- 0.00117396
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54518 +/- 6.26359e-06
  return (TDataSet *)tableSet;// West = 5.54528 +/- 6.62035e-06 East = 5.54433 +/- 1.93416e-05
};
