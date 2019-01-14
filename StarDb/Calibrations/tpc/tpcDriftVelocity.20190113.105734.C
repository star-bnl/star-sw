TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 13019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54198; // +/- 1.36302e-05 cm/us All: East = 0.504654 +/- 0.00504706
  row.laserDriftVelocityWest	 =   5.54198; // +/- 1.36302e-05 cm/us All: West = 1.0568 +/- 0.00280638
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54198 +/- 1.36302e-05
  return (TDataSet *)tableSet;// West = 5.54124 +/- 1.56399e-05 East = 5.54429 +/- 2.7795e-05
};
