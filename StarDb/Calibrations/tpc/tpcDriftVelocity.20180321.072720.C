TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56131; // +/- 1.08377e-05 cm/us All: East = 1.58265 +/- 0.0165183
  row.laserDriftVelocityWest	 =   5.56131; // +/- 1.08377e-05 cm/us All: West = 0.163604 +/- 0.00191776
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56131 +/- 1.08377e-05
  return (TDataSet *)tableSet;// West = 5.56143 +/- 1.09221e-05 East = 5.5534 +/- 8.72976e-05
};
