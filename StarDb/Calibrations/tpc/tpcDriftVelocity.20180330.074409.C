TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52653; // +/- 4.66497e-05 cm/us All: East = 0.690736 +/- 4.62206
  row.laserDriftVelocityWest	 =   5.52653; // +/- 4.66497e-05 cm/us All: West = 0.165148 +/- 0.0157043
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52653 +/- 4.66497e-05
  return (TDataSet *)tableSet;// West = 5.52653 +/- 4.66498e-05 East = 5.52376 +/- 0.0222241
};
