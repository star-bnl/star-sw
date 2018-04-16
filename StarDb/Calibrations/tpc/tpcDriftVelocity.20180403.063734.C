TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52971; // +/- 9.07685e-06 cm/us All: East = -0.873191 +/- 0.00433625
  row.laserDriftVelocityWest	 =   5.52971; // +/- 9.07685e-06 cm/us All: West = -0.0160307 +/- 0.00173812
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52971 +/- 9.07685e-06
  return (TDataSet *)tableSet;// West = 5.52911 +/- 9.71488e-06 East = 5.53382 +/- 2.54665e-05
};
