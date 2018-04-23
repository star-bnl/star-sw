TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5416; // +/- 1.73721e-05 cm/us All: East = -0.313007 +/- 0.00584086
  row.laserDriftVelocityWest	 =   5.5416; // +/- 1.73721e-05 cm/us All: West = 0.412188 +/- 0.00375703
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5416 +/- 1.73721e-05
  return (TDataSet *)tableSet;// West = 5.54043 +/- 2.0691e-05 East = 5.54439 +/- 3.19806e-05
};
