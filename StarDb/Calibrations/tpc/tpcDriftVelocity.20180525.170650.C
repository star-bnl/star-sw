TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55225; // +/- 0.000132199 cm/us All: East = -0.0786111 +/- 0.758368
  row.laserDriftVelocityWest	 =   5.55225; // +/- 0.000132199 cm/us All: West = 0.315211 +/- 0.621538
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55225 +/- 0.000132199
  return (TDataSet *)tableSet;// West = 5.55196 +/- 0.000143391 East = 5.55391 +/- 0.000341316
};
