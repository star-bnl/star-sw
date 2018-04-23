TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55214; // +/- 7.87358e-06 cm/us All: East = -0.063024 +/- 0.0041237
  row.laserDriftVelocityWest	 =   5.55214; // +/- 7.87358e-06 cm/us All: West = 0.598755 +/- 0.00148851
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55214 +/- 7.87358e-06
  return (TDataSet *)tableSet;// West = 5.55174 +/- 8.38428e-06 East = 5.55514 +/- 2.29098e-05
};
