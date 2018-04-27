TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54154; // +/- 1.70605e-05 cm/us All: East = -0.437112 +/- 0.00576168
  row.laserDriftVelocityWest	 =   5.54154; // +/- 1.70605e-05 cm/us All: West = 0.263443 +/- 0.00366446
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54154 +/- 1.70605e-05
  return (TDataSet *)tableSet;// West = 5.54047 +/- 2.01025e-05 East = 5.5443 +/- 3.22556e-05
};
