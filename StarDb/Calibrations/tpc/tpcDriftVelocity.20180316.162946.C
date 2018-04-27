TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53132; // +/- 9.90835e-06 cm/us All: East = 1.69674 +/- 31.5505
  row.laserDriftVelocityWest	 =   5.53132; // +/- 9.90835e-06 cm/us All: West = 2.03309 +/- 0.00174888
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53132 +/- 9.90835e-06
  return (TDataSet *)tableSet;// West = 5.53132 +/- 9.90835e-06 East = 5.53313 +/- 0.176333
};
