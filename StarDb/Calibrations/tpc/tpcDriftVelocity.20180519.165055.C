TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55769; // +/- 8.4664e-06 cm/us All: East = -0.710415 +/- 0.00408794
  row.laserDriftVelocityWest	 =   5.55769; // +/- 8.4664e-06 cm/us All: West = -0.368123 +/- 0.00160456
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55769 +/- 8.4664e-06
  return (TDataSet *)tableSet;// West = 5.55745 +/- 9.09488e-06 East = 5.55923 +/- 2.31777e-05
};
