TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 152029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55087; // +/- 7.69569e-05 cm/us All: East = 0.147391 +/- 0.16997
  row.laserDriftVelocityWest	 =   5.55087; // +/- 7.69569e-05 cm/us All: West = 0.210474 +/- 0.0930232
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55087 +/- 7.69569e-05
  return (TDataSet *)tableSet;// West = 5.55109 +/- 9.96411e-05 East = 5.55054 +/- 0.000121152
};
