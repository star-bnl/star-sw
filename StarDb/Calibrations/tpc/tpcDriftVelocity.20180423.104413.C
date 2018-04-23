TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 113015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5507; // +/- 7.95699e-06 cm/us All: East = 0.43477 +/- 0.00301871
  row.laserDriftVelocityWest	 =   5.5507; // +/- 7.95699e-06 cm/us All: West = 0.879943 +/- 0.00160981
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5507 +/- 7.95699e-06
  return (TDataSet *)tableSet;// West = 5.55016 +/- 9.08159e-06 East = 5.55249 +/- 1.6508e-05
};
