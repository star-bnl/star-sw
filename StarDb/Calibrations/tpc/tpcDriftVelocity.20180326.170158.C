TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51848; // +/- 1.19467e-05 cm/us All: East = 4.04021 +/- 0.00655625
  row.laserDriftVelocityWest	 =   5.51848; // +/- 1.19467e-05 cm/us All: West = 4.28544 +/- 0.0023364
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51848 +/- 1.19467e-05
  return (TDataSet *)tableSet;// West = 5.51831 +/- 1.27734e-05 East = 5.51968 +/- 3.37563e-05
};
