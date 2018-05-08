TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52846; // +/- 2.29173e-05 cm/us All: East = -0.48842 +/- 0.0290173
  row.laserDriftVelocityWest	 =   5.52846; // +/- 2.29173e-05 cm/us All: West = 0.0643142 +/- 0.00429354
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52846 +/- 2.29173e-05
  return (TDataSet *)tableSet;// West = 5.52835 +/- 2.3349e-05 East = 5.53134 +/- 0.000119738
};
