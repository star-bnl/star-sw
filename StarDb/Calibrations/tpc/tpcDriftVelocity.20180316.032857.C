TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74111
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53437; // +/- 1.05341e-05 cm/us All: East = -0.137674 +/- 0.00995471
  row.laserDriftVelocityWest	 =   5.53437; // +/- 1.05341e-05 cm/us All: West = 0.20027 +/- 0.00191912
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53437 +/- 1.05341e-05
  return (TDataSet *)tableSet;// West = 5.5343 +/- 1.07254e-05 East = 5.53616 +/- 5.60318e-05
};
