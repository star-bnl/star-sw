TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52732; // +/- 1.11789e-05 cm/us All: East = -2.82972 +/- 0.0127494
  row.laserDriftVelocityWest	 =   5.52732; // +/- 1.11789e-05 cm/us All: West = -1.963 +/- 0.00201467
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52732 +/- 1.11789e-05
  return (TDataSet *)tableSet;// West = 5.52721 +/- 1.13209e-05 East = 5.53183 +/- 7.08114e-05
};
