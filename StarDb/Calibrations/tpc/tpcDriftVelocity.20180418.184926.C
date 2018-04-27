TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54438; // +/- 8.49198e-06 cm/us All: East = -0.115351 +/- 0.00310319
  row.laserDriftVelocityWest	 =   5.54438; // +/- 8.49198e-06 cm/us All: West = 0.203893 +/- 0.00175703
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54438 +/- 8.49198e-06
  return (TDataSet *)tableSet;// West = 5.54394 +/- 9.82159e-06 East = 5.54566 +/- 1.69021e-05
};
