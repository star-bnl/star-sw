TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54211; // +/- 7.9197e-06 cm/us All: East = -0.347836 +/- 0.00430505
  row.laserDriftVelocityWest	 =   5.54211; // +/- 7.9197e-06 cm/us All: West = 0.229918 +/- 0.00150909
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54211 +/- 7.9197e-06
  return (TDataSet *)tableSet;// West = 5.54176 +/- 8.43475e-06 East = 5.54469 +/- 2.30165e-05
};
