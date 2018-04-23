TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52792; // +/- 5.30026e-05 cm/us All: East = -0.463158 +/- 0.259998
  row.laserDriftVelocityWest	 =   5.52792; // +/- 5.30026e-05 cm/us All: West = 0.260502 +/- 0.019761
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52792 +/- 5.30026e-05
  return (TDataSet *)tableSet;// West = 5.5277 +/- 5.45616e-05 East = 5.53169 +/- 0.000223317
};
