TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5481; // +/- 1.33131e-05 cm/us All: East = -5.55114 +/- 0.0109835
  row.laserDriftVelocityWest	 =   5.5481; // +/- 1.33131e-05 cm/us All: West = -4.53416 +/- 0.00243846
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5481 +/- 1.33131e-05
  return (TDataSet *)tableSet;// West = 5.54776 +/- 1.37174e-05 East = 5.55348 +/- 5.52453e-05
};
