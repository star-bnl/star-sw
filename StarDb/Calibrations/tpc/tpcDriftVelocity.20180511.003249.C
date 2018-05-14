TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130073
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55201; // +/- 4.03684e-05 cm/us All: East = -0.30704 +/- 0.137854
  row.laserDriftVelocityWest	 =   5.55201; // +/- 4.03684e-05 cm/us All: West = -0.0258004 +/- 0.00563286
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55201 +/- 4.03684e-05
  return (TDataSet *)tableSet;// West = 5.55056 +/- 5.01006e-05 East = 5.55471 +/- 6.81598e-05
};
