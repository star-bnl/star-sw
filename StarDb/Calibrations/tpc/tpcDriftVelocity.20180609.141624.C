TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54946; // +/- 5.97736e-06 cm/us All: East = -0.0160629 +/- 0.00299094
  row.laserDriftVelocityWest	 =   5.54946; // +/- 5.97736e-06 cm/us All: West = 0.156252 +/- 0.00113865
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54946 +/- 5.97736e-06
  return (TDataSet *)tableSet;// West = 5.54935 +/- 6.39632e-06 East = 5.55024 +/- 1.6792e-05
};
