TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54547; // +/- 1.30154e-05 cm/us All: East = -0.278008 +/- 0.00401627
  row.laserDriftVelocityWest	 =   5.54547; // +/- 1.30154e-05 cm/us All: West = 0.0958269 +/- 0.00295183
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54547 +/- 1.30154e-05
  return (TDataSet *)tableSet;// West = 5.54474 +/- 1.62993e-05 East = 5.54677 +/- 2.16218e-05
};
