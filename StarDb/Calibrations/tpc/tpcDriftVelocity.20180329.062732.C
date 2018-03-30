TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 88006
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52774; // +/- 8.43907e-06 cm/us All: East = 1.80129 +/- 0.00464325
  row.laserDriftVelocityWest	 =   5.52774; // +/- 8.43907e-06 cm/us All: West = 2.66376 +/- 0.00160882
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52774 +/- 8.43907e-06
  return (TDataSet *)tableSet;// West = 5.52726 +/- 8.89449e-06 East = 5.53204 +/- 2.67157e-05
};
