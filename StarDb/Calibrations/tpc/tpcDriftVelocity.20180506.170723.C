TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 126034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56437; // +/- 6.4116e-06 cm/us All: East = 0.0682637 +/- 0.00162782
  row.laserDriftVelocityWest	 =   5.56437; // +/- 6.4116e-06 cm/us All: West = 0.248326 +/- 0.00159137
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56437 +/- 6.4116e-06
  return (TDataSet *)tableSet;// West = 5.56389 +/- 8.93176e-06 East = 5.56488 +/- 9.20935e-06
};
