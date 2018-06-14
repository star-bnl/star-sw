TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 158060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55088; // +/- 5.50112e-06 cm/us All: East = -0.0203418 +/- 0.00199182
  row.laserDriftVelocityWest	 =   5.55088; // +/- 5.50112e-06 cm/us All: West = 0.197733 +/- 0.00113142
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55088 +/- 5.50112e-06
  return (TDataSet *)tableSet;// West = 5.55056 +/- 6.35096e-06 East = 5.55185 +/- 1.10084e-05
};
