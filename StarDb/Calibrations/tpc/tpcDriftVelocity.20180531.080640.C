TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 151036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54977; // +/- 1.02858e-05 cm/us All: East = -0.123179 +/- 0.00382502
  row.laserDriftVelocityWest	 =   5.54977; // +/- 1.02858e-05 cm/us All: West = 0.255512 +/- 0.00208846
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54977 +/- 1.02858e-05
  return (TDataSet *)tableSet;// West = 5.54932 +/- 1.17063e-05 East = 5.55129 +/- 2.15424e-05
};
