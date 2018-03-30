TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53246; // +/- 9.33378e-06 cm/us All: East = 1.33818 +/- 0.00609116
  row.laserDriftVelocityWest	 =   5.53246; // +/- 9.33378e-06 cm/us All: West = 1.75002 +/- 0.0017579
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53246 +/- 9.33378e-06
  return (TDataSet *)tableSet;// West = 5.53229 +/- 9.71325e-06 East = 5.53445 +/- 3.37228e-05
};
