TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 125050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55663; // +/- 7.74049e-05 cm/us All: East = 0.269686 +/- 0.284987
  row.laserDriftVelocityWest	 =   5.55663; // +/- 7.74049e-05 cm/us All: West = 0.182492 +/- 0.0815292
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55663 +/- 7.74049e-05
  return (TDataSet *)tableSet;// West = 5.55669 +/- 8.27176e-05 East = 5.55624 +/- 0.000219524
};
