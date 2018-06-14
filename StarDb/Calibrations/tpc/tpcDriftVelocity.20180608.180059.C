TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55; // +/- 5.24269e-06 cm/us All: East = 0.805051 +/- 0.00184294
  row.laserDriftVelocityWest	 =   5.55; // +/- 5.24269e-06 cm/us All: West = 0.722224 +/- 0.00108465
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55 +/- 5.24269e-06
  return (TDataSet *)tableSet;// West = 5.55012 +/- 6.10652e-06 East = 5.54965 +/- 1.02248e-05
};
