TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80092
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55181; // +/- 1.28368e-05 cm/us All: East = 0.239721 +/- 0.516429
  row.laserDriftVelocityWest	 =   5.55181; // +/- 1.28368e-05 cm/us All: West = 0.175585 +/- 0.00234252
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55181 +/- 1.28368e-05
  return (TDataSet *)tableSet;// West = 5.55181 +/- 1.28801e-05 East = 5.55155 +/- 0.000156773
};
