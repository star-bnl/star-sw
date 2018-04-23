TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5556; // +/- 1.3071e-05 cm/us All: East = -0.095924 +/- 0.00399891
  row.laserDriftVelocityWest	 =   5.5556; // +/- 1.3071e-05 cm/us All: West = 0.32458 +/- 0.00301804
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5556 +/- 1.3071e-05
  return (TDataSet *)tableSet;// West = 5.55474 +/- 1.65993e-05 East = 5.557 +/- 2.12059e-05
};
