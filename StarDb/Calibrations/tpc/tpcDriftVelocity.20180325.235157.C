TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5311; // +/- 1.11207e-05 cm/us All: East = -0.224548 +/- 0.00970764
  row.laserDriftVelocityWest	 =   5.5311; // +/- 1.11207e-05 cm/us All: West = 0.223149 +/- 0.0020403
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5311 +/- 1.11207e-05
  return (TDataSet *)tableSet;// West = 5.53098 +/- 1.13873e-05 East = 5.53341 +/- 5.16936e-05
};
