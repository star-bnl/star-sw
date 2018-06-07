TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 146025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.552; // +/- 9.73315e-05 cm/us All: East = 0.0490323 +/- 0.290322
  row.laserDriftVelocityWest	 =   5.552; // +/- 9.73315e-05 cm/us All: West = -0.0278169 +/- 0.0281436
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.552 +/- 9.73315e-05
  return (TDataSet *)tableSet;// West = 5.55152 +/- 0.000124673 East = 5.55274 +/- 0.000155752
};
