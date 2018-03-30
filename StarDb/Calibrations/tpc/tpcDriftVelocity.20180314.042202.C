TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 73004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55125; // +/- 1.01515e-05 cm/us All: East = -1.77502 +/- 0.00602529
  row.laserDriftVelocityWest	 =   5.55125; // +/- 1.01515e-05 cm/us All: West = -1.6645 +/- 0.00190501
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55125 +/- 1.01515e-05
  return (TDataSet *)tableSet;// West = 5.55119 +/- 1.06497e-05 East = 5.55187 +/- 3.35843e-05
};
