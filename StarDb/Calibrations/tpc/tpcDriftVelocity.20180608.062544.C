TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5505; // +/- 5.34773e-06 cm/us All: East = -0.0354248 +/- 0.00330538
  row.laserDriftVelocityWest	 =   5.5505; // +/- 5.34773e-06 cm/us All: West = 0.191144 +/- 0.000992144
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5505 +/- 5.34773e-06
  return (TDataSet *)tableSet;// West = 5.55041 +/- 5.5963e-06 East = 5.55149 +/- 1.81449e-05
};
