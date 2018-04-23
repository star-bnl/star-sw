TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52696; // +/- 1.37879e-05 cm/us All: East = -0.588849 +/- 0.00728244
  row.laserDriftVelocityWest	 =   5.52696; // +/- 1.37879e-05 cm/us All: West = 0.279309 +/- 0.00265358
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52696 +/- 1.37879e-05
  return (TDataSet *)tableSet;// West = 5.52642 +/- 1.46467e-05 East = 5.5312 +/- 4.08659e-05
};
