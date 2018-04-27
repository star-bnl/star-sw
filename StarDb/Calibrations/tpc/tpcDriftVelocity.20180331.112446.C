TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51689; // +/- 7.46318e-05 cm/us All: East = -0.880177 +/- 1.22657
  row.laserDriftVelocityWest	 =   5.51689; // +/- 7.46318e-05 cm/us All: West = 0.108951 +/- 0.0531935
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51689 +/- 7.46318e-05
  return (TDataSet *)tableSet;// West = 5.51689 +/- 7.46781e-05 East = 5.52244 +/- 0.00211955
};
