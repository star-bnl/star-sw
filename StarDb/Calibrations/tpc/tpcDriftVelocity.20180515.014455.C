TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54619; // +/- 4.86775e-06 cm/us All: East = 0.250192 +/- 0.0019242
  row.laserDriftVelocityWest	 =   5.54619; // +/- 4.86775e-06 cm/us All: West = 0.146632 +/- 0.000966206
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54619 +/- 4.86775e-06
  return (TDataSet *)tableSet;// West = 5.5463 +/- 5.46276e-06 East = 5.54575 +/- 1.07255e-05
};
