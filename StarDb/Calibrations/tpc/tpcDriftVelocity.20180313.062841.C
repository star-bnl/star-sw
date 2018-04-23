TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54182; // +/- 2.93868e-05 cm/us All: East = 0.244423 +/- 0.0734383
  row.laserDriftVelocityWest	 =   5.54182; // +/- 2.93868e-05 cm/us All: West = 0.235746 +/- 0.0057554
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54182 +/- 2.93868e-05
  return (TDataSet *)tableSet;// West = 5.54177 +/- 3.0209e-05 East = 5.5428 +/- 0.000126822
};
