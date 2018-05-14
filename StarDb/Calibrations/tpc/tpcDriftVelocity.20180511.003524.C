TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130074
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55156; // +/- 6.37821e-05 cm/us All: East = -0.000322602 +/- 0.467535
  row.laserDriftVelocityWest	 =   5.55156; // +/- 6.37821e-05 cm/us All: West = 0.0177596 +/- 0.00666313
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55156 +/- 6.37821e-05
  return (TDataSet *)tableSet;// West = 5.55069 +/- 7.16972e-05 East = 5.55486 +/- 0.000139648
};
