TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54599; // +/- 5.56906e-06 cm/us All: East = 0.0456197 +/- 0.00959782
  row.laserDriftVelocityWest	 =   5.54599; // +/- 5.56906e-06 cm/us All: West = 0.192912 +/- 0.000987199
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54599 +/- 5.56906e-06
  return (TDataSet *)tableSet;// West = 5.54598 +/- 5.60606e-06 East = 5.54685 +/- 4.85552e-05
};
