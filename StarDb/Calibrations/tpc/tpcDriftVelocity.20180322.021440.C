TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80092
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5518; // +/- 1.28477e-05 cm/us All: East = -1.8942 +/- 0.42336
  row.laserDriftVelocityWest	 =   5.5518; // +/- 1.28477e-05 cm/us All: West = -1.76814 +/- 0.00231338
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5518 +/- 1.28477e-05
  return (TDataSet *)tableSet;// West = 5.5518 +/- 1.28986e-05 East = 5.55146 +/- 0.000144738
};
