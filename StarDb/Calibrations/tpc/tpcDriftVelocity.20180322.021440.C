TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80092
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54182; // +/- 1.27426e-05 cm/us All: East = 1.60169 +/- 0.323156
  row.laserDriftVelocityWest	 =   5.54182; // +/- 1.27426e-05 cm/us All: West = 1.88425 +/- 0.00230757
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54182 +/- 1.27426e-05
  return (TDataSet *)tableSet;// West = 5.54182 +/- 1.27838e-05 East = 5.54236 +/- 0.000158866
};
