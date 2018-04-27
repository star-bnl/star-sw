TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54414; // +/- 9.68917e-06 cm/us All: East = -0.176835 +/- 0.00319633
  row.laserDriftVelocityWest	 =   5.54414; // +/- 9.68917e-06 cm/us All: West = 0.139185 +/- 0.00208969
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54414 +/- 9.68917e-06
  return (TDataSet *)tableSet;// West = 5.5436 +/- 1.16596e-05 East = 5.54533 +/- 1.74181e-05
};
