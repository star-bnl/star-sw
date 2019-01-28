TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 28041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5276; // +/- 1.23178e-05 cm/us All: East = 0.118547 +/- 0.00346991
  row.laserDriftVelocityWest	 =   5.5276; // +/- 1.23178e-05 cm/us All: West = 0.214922 +/- 0.00279733
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5276 +/- 1.23178e-05
  return (TDataSet *)tableSet;// West = 5.52744 +/- 1.58438e-05 East = 5.52784 +/- 1.95851e-05
};
