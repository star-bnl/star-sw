TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5219; // +/- 2.46311e-05 cm/us All: East = -0.0270464 +/- 0.0129179
  row.laserDriftVelocityWest	 =   5.5219; // +/- 2.46311e-05 cm/us All: West = 0.164984 +/- 0.00525873
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5219 +/- 2.46311e-05
  return (TDataSet *)tableSet;// West = 5.52156 +/- 2.81889e-05 East = 5.523 +/- 5.06497e-05
};
