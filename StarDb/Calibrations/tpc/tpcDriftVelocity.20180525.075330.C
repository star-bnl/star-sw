TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55059; // +/- 0.000123499 cm/us All: East = -0.1605 +/- 1.07694
  row.laserDriftVelocityWest	 =   5.55059; // +/- 0.000123499 cm/us All: West = 0.195864 +/- 0.0867602
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55059 +/- 0.000123499
  return (TDataSet *)tableSet;// West = 5.55051 +/- 0.000125376 East = 5.55335 +/- 0.000716423
};
