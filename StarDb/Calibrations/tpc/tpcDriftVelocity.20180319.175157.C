TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55336; // +/- 1.13102e-05 cm/us All: East = -0.621028 +/- 0.0155115
  row.laserDriftVelocityWest	 =   5.55336; // +/- 1.13102e-05 cm/us All: West = 0.171343 +/- 0.00203739
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55336 +/- 1.13102e-05
  return (TDataSet *)tableSet;// West = 5.55326 +/- 1.14323e-05 East = 5.55779 +/- 7.75824e-05
};
