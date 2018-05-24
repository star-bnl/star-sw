TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55573; // +/- 5.51534e-06 cm/us All: East = 0.205081 +/- 0.00405598
  row.laserDriftVelocityWest	 =   5.55573; // +/- 5.51534e-06 cm/us All: West = -0.0619711 +/- 0.0010054
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55573 +/- 5.51534e-06
  return (TDataSet *)tableSet;// West = 5.55582 +/- 5.68445e-06 East = 5.55426 +/- 2.27809e-05
};
