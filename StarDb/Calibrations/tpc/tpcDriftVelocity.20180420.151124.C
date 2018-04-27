TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5438; // +/- 6.86175e-06 cm/us All: East = 0.0170689 +/- 0.00164092
  row.laserDriftVelocityWest	 =   5.5438; // +/- 6.86175e-06 cm/us All: West = 0.252036 +/- 0.0011213
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5438 +/- 6.86175e-06
  return (TDataSet *)tableSet;// West = 5.54279 +/- 1.0294e-05 East = 5.54461 +/- 9.20498e-06
};
