TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52943; // +/- 7.76963e-06 cm/us All: East = -0.597877 +/- 0.0043047
  row.laserDriftVelocityWest	 =   5.52943; // +/- 7.76963e-06 cm/us All: West = 0.204554 +/- 0.00146923
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52943 +/- 7.76963e-06
  return (TDataSet *)tableSet;// West = 5.52899 +/- 8.18584e-06 East = 5.53343 +/- 2.46805e-05
};
