TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54691; // +/- 0.00011557 cm/us All: East = -0.233828 +/- 0.498605
  row.laserDriftVelocityWest	 =   5.54691; // +/- 0.00011557 cm/us All: West = 0.255744 +/- 0.0482651
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54691 +/- 0.00011557
  return (TDataSet *)tableSet;// West = 5.54685 +/- 0.000116688 East = 5.54975 +/- 0.000836783
};
