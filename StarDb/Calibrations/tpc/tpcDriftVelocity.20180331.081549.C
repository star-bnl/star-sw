TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52721; // +/- 1.01039e-05 cm/us All: East = -3.3033 +/- 0.45093
  row.laserDriftVelocityWest	 =   5.52721; // +/- 1.01039e-05 cm/us All: West = -1.95743 +/- 0.00181974
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52721 +/- 1.01039e-05
  return (TDataSet *)tableSet;// West = 5.52717 +/- 1.01343e-05 East = 5.5341 +/- 0.000130535
};
