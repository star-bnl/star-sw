TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54536; // +/- 1.1144e-05 cm/us All: East = 0.427207 +/- 1.13515
  row.laserDriftVelocityWest	 =   5.54536; // +/- 1.1144e-05 cm/us All: West = 0.206644 +/- 0.00200345
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54536 +/- 1.1144e-05
  return (TDataSet *)tableSet;// West = 5.54536 +/- 1.11442e-05 East = 5.54407 +/- 0.00174073
};
