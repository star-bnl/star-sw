TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 127013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.571; // +/- 6.02088e-05 cm/us All: East = -0.092791 +/- 0.281807
  row.laserDriftVelocityWest	 =   5.571; // +/- 6.02088e-05 cm/us All: West = 0.0274695 +/- 0.197348
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.571 +/- 6.02088e-05
  return (TDataSet *)tableSet;// West = 5.57063 +/- 0.00011688 East = 5.57113 +/- 7.02463e-05
};
