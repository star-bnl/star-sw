TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 157013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55329; // +/- 5.11262e-06 cm/us All: East = 0.128723 +/- 0.0026368
  row.laserDriftVelocityWest	 =   5.55329; // +/- 5.11262e-06 cm/us All: West = 0.156027 +/- 0.000957575
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55329 +/- 5.11262e-06
  return (TDataSet *)tableSet;// West = 5.55327 +/- 5.45315e-06 East = 5.55342 +/- 1.46982e-05
};
