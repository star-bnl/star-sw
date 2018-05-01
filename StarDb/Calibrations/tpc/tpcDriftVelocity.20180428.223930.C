TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 118047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53752; // +/- 1.43766e-05 cm/us All: East = -0.954809 +/- 0.0105699
  row.laserDriftVelocityWest	 =   5.53752; // +/- 1.43766e-05 cm/us All: West = 0.149258 +/- 0.00271958
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53752 +/- 1.43766e-05
  return (TDataSet *)tableSet;// West = 5.53705 +/- 1.4962e-05 East = 5.54321 +/- 5.19004e-05
};
