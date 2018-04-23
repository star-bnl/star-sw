TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52821; // +/- 8.45923e-06 cm/us All: East = -0.740119 +/- 0.00850175
  row.laserDriftVelocityWest	 =   5.52821; // +/- 8.45923e-06 cm/us All: West = 0.211262 +/- 0.00153088
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52821 +/- 8.45923e-06
  return (TDataSet *)tableSet;// West = 5.52806 +/- 8.58539e-06 East = 5.5332 +/- 4.9527e-05
};
