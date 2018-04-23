TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53435; // +/- 1.02025e-05 cm/us All: East = 1.07172 +/- 2.3364
  row.laserDriftVelocityWest	 =   5.53435; // +/- 1.02025e-05 cm/us All: West = 0.179566 +/- 0.00182694
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53435 +/- 1.02025e-05
  return (TDataSet *)tableSet;// West = 5.53435 +/- 1.02027e-05 East = 5.53761 +/- 0.0016541
};
