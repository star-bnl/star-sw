TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55184; // +/- 8.33633e-06 cm/us All: East = -0.133643 +/- 0.00231248
  row.laserDriftVelocityWest	 =   5.55184; // +/- 8.33633e-06 cm/us All: West = 0.400221 +/- 0.00191072
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55184 +/- 8.33633e-06
  return (TDataSet *)tableSet;// West = 5.55057 +/- 1.06906e-05 East = 5.5538 +/- 1.33157e-05
};
