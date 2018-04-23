TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52162; // +/- 1.37057e-05 cm/us All: East = -0.787757 +/- 0.00737377
  row.laserDriftVelocityWest	 =   5.52162; // +/- 1.37057e-05 cm/us All: West = 0.323911 +/- 0.00265178
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52162 +/- 1.37057e-05
  return (TDataSet *)tableSet;// West = 5.52099 +/- 1.44934e-05 East = 5.52702 +/- 4.21472e-05
};
