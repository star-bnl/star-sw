TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55648; // +/- 8.63694e-06 cm/us All: East = 2.24756 +/- 0.0136102
  row.laserDriftVelocityWest	 =   5.55648; // +/- 8.63694e-06 cm/us All: West = 1.93007 +/- 0.00155383
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55648 +/- 8.63694e-06
  return (TDataSet *)tableSet;// West = 5.55652 +/- 8.7148e-06 East = 5.55464 +/- 6.4757e-05
};
