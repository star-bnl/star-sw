TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 22019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56271; // +/- 9.2532e-06 cm/us All: East = -1.1141 +/- 0.0179903
  row.laserDriftVelocityWest	 =   5.56271; // +/- 9.2532e-06 cm/us All: West = -0.295017 +/- 0.00165038
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56271 +/- 9.2532e-06
  return (TDataSet *)tableSet;// West = 5.56264 +/- 9.34253e-06 East = 5.56631 +/- 6.7072e-05
};
