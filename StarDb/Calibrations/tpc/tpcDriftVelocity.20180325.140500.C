TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53241; // +/- 9.40717e-06 cm/us All: East = -0.194508 +/- 0.00618468
  row.laserDriftVelocityWest	 =   5.53241; // +/- 9.40717e-06 cm/us All: West = 0.220961 +/- 0.00173922
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53241 +/- 9.40717e-06
  return (TDataSet *)tableSet;// West = 5.53224 +/- 9.78904e-06 East = 5.53448 +/- 3.40121e-05
};
