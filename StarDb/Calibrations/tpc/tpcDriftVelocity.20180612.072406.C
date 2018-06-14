TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54672; // +/- 0.000258898 cm/us All: East = -0.559369 +/- 1.06325
  row.laserDriftVelocityWest	 =   5.54672; // +/- 0.000258898 cm/us All: West = 0.666089 +/- 0.705427
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54672 +/- 0.000258898
  return (TDataSet *)tableSet;// West = 5.54671 +/- 0.000259088 East = 5.55297 +/- 0.00675951
};
