TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53471; // +/- 9.18165e-06 cm/us All: East = 0.0205094 +/- 0.00215985
  row.laserDriftVelocityWest	 =   5.53471; // +/- 9.18165e-06 cm/us All: West = 0.232757 +/- 0.00254665
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53471 +/- 9.18165e-06
  return (TDataSet *)tableSet;// West = 5.53403 +/- 1.41594e-05 East = 5.5352 +/- 1.20612e-05
};
