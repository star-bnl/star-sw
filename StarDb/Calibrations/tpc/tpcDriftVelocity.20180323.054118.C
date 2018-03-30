TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54064; // +/- 1.50117e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54064; // +/- 1.50117e-05 cm/us All: West = 0.242255 +/- 0.00268118
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54064 +/- 1.50117e-05
  return (TDataSet *)tableSet;// West = 5.54064 +/- 1.50117e-05 East = -999 +/- 999
};
