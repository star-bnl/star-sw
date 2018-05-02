TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53421; // +/- 2.36796e-05 cm/us All: East = -0.775412 +/- 0.268001
  row.laserDriftVelocityWest	 =   5.53421; // +/- 2.36796e-05 cm/us All: West = 0.260956 +/- 0.00458541
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53421 +/- 2.36796e-05
  return (TDataSet *)tableSet;// West = 5.53377 +/- 2.45283e-05 East = 5.54031 +/- 9.08054e-05
};
