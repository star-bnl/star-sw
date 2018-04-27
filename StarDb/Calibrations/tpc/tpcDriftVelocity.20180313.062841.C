TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53187; // +/- 2.87922e-05 cm/us All: East = 1.96512 +/- 0.0591186
  row.laserDriftVelocityWest	 =   5.53187; // +/- 2.87922e-05 cm/us All: West = 2.03161 +/- 0.00546625
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53187 +/- 2.87922e-05
  return (TDataSet *)tableSet;// West = 5.53181 +/- 2.9287e-05 East = 5.53363 +/- 0.000157301
};
