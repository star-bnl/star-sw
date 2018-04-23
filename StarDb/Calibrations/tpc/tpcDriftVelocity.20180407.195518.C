TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54631; // +/- 9.23634e-06 cm/us All: East = 0.140718 +/- 0.00234523
  row.laserDriftVelocityWest	 =   5.54631; // +/- 9.23634e-06 cm/us All: West = 0.236329 +/- 0.00231543
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54631 +/- 9.23634e-06
  return (TDataSet *)tableSet;// West = 5.54603 +/- 1.30005e-05 East = 5.54659 +/- 1.31247e-05
};
