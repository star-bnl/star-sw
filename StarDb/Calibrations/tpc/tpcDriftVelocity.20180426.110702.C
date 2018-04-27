TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 116017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54356; // +/- 6.80365e-06 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54356; // +/- 6.80365e-06 cm/us All: West = 0.176463 +/- 0.00119964
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54356 +/- 6.80365e-06
  return (TDataSet *)tableSet;// West = 5.54356 +/- 6.80365e-06 East = -999 +/- 999
};
