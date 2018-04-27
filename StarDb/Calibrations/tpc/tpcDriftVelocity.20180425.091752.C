TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 115012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54257; // +/- 1.50564e-05 cm/us All: East = -0.693577 +/- 0.0247498
  row.laserDriftVelocityWest	 =   5.54257; // +/- 1.50564e-05 cm/us All: West = 0.156769 +/- 0.00278495
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54257 +/- 1.50564e-05
  return (TDataSet *)tableSet;// West = 5.54244 +/- 1.52565e-05 East = 5.54729 +/- 9.32656e-05
};
