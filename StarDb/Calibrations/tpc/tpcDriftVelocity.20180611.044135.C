TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54743; // +/- 8.64498e-06 cm/us All: East = 0.136178 +/- 0.00345904
  row.laserDriftVelocityWest	 =   5.54743; // +/- 8.64498e-06 cm/us All: West = 0.255376 +/- 0.00174869
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54743 +/- 8.64498e-06
  return (TDataSet *)tableSet;// West = 5.54729 +/- 9.79307e-06 East = 5.54794 +/- 1.84008e-05
};
