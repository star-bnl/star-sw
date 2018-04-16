TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55119; // +/- 1.54825e-05 cm/us All: East = -5.31988 +/- 0.00371104
  row.laserDriftVelocityWest	 =   5.55119; // +/- 1.54825e-05 cm/us All: West = -4.95019 +/- 0.00428413
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55119 +/- 1.54825e-05
  return (TDataSet *)tableSet;// West = 5.55008 +/- 2.37114e-05 East = 5.55201 +/- 2.04417e-05
};
