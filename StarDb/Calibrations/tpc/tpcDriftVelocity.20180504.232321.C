TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53649; // +/- 9.19633e-06 cm/us All: East = 0.205851 +/- 0.00203176
  row.laserDriftVelocityWest	 =   5.53649; // +/- 9.19633e-06 cm/us All: West = 0.225994 +/- 0.00153233
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53649 +/- 9.19633e-06
  return (TDataSet *)tableSet;// West = 5.53585 +/- 1.55404e-05 East = 5.53683 +/- 1.14083e-05
};
