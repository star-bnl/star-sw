TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52404; // +/- 7.97106e-06 cm/us All: East = 3.08007 +/- 0.00576487
  row.laserDriftVelocityWest	 =   5.52404; // +/- 7.97106e-06 cm/us All: West = 3.25457 +/- 0.0014805
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52404 +/- 7.97106e-06
  return (TDataSet *)tableSet;// West = 5.52398 +/- 8.24021e-06 East = 5.52496 +/- 3.14449e-05
};
