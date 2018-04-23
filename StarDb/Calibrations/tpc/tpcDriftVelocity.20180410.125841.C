TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55496; // +/- 3.14926e-05 cm/us All: East = -0.0180059 +/- 0.00555098
  row.laserDriftVelocityWest	 =   5.55496; // +/- 3.14926e-05 cm/us All: West = 1.47442 +/- 0.139345
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55496 +/- 3.14926e-05
  return (TDataSet *)tableSet;// West = 5.54728 +/- 0.000213365 East = 5.55513 +/- 3.18413e-05
};
