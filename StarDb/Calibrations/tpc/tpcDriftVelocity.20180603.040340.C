TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5528; // +/- 6.94241e-06 cm/us All: East = 0.0657881 +/- 0.0108606
  row.laserDriftVelocityWest	 =   5.5528; // +/- 6.94241e-06 cm/us All: West = 0.153298 +/- 0.00125395
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5528 +/- 6.94241e-06
  return (TDataSet *)tableSet;// West = 5.55279 +/- 7.02631e-06 East = 5.55302 +/- 4.50571e-05
};
