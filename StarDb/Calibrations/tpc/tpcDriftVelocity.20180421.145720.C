TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55364; // +/- 6.10006e-06 cm/us All: East = 0.0355292 +/- 0.00210886
  row.laserDriftVelocityWest	 =   5.55364; // +/- 6.10006e-06 cm/us All: West = 0.236438 +/- 0.00125493
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55364 +/- 6.10006e-06
  return (TDataSet *)tableSet;// West = 5.55334 +/- 7.13254e-06 East = 5.55448 +/- 1.17711e-05
};
