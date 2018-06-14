TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55029; // +/- 4.81431e-06 cm/us All: East = 0.175587 +/- 0.00177506
  row.laserDriftVelocityWest	 =   5.55029; // +/- 4.81431e-06 cm/us All: West = 0.0942271 +/- 0.000974294
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55029 +/- 4.81431e-06
  return (TDataSet *)tableSet;// West = 5.55039 +/- 5.51109e-06 East = 5.54995 +/- 9.8917e-06
};
