TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55524; // +/- 2.03973e-05 cm/us All: East = -5.98929 +/- 0.00471523
  row.laserDriftVelocityWest	 =   5.55524; // +/- 2.03973e-05 cm/us All: West = -5.60372 +/- 0.00704021
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55524 +/- 2.03973e-05
  return (TDataSet *)tableSet;// West = 5.55379 +/- 3.63002e-05 East = 5.55592 +/- 2.46583e-05
};
