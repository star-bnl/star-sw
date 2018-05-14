TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130075
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55097; // +/- 2.10801e-05 cm/us All: East = 0.325694 +/- 0.011859
  row.laserDriftVelocityWest	 =   5.55097; // +/- 2.10801e-05 cm/us All: West = 0.126732 +/- 0.0022867
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55097 +/- 2.10801e-05
  return (TDataSet *)tableSet;// West = 5.55055 +/- 2.43879e-05 East = 5.55222 +/- 4.19206e-05
};
