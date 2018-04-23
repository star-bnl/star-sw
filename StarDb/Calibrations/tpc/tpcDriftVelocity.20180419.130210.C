TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55422; // +/- 9.99434e-06 cm/us All: East = -0.197509 +/- 0.00333477
  row.laserDriftVelocityWest	 =   5.55422; // +/- 9.99434e-06 cm/us All: West = 0.185821 +/- 0.00212326
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55422 +/- 9.99434e-06
  return (TDataSet *)tableSet;// West = 5.55362 +/- 1.19006e-05 East = 5.55566 +/- 1.84101e-05
};
