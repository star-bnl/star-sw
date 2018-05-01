TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 54086
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50749; // +/- 1.49957e-05 cm/us All: East = 0.0058069 +/- 0.00550027
  row.laserDriftVelocityWest	 =   5.50749; // +/- 1.49957e-05 cm/us All: West = 0.264169 +/- 0.00330492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50749 +/- 1.49957e-05
  return (TDataSet *)tableSet;// West = 5.50714 +/- 1.81226e-05 East = 5.50824 +/- 2.67054e-05
};
