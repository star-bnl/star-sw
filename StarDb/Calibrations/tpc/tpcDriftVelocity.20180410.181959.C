TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54424; // +/- 1.21646e-05 cm/us All: East = -0.409958 +/- 0.00218505
  row.laserDriftVelocityWest	 =   5.54424; // +/- 1.21646e-05 cm/us All: West = 0.914241 +/- 1.59141
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54424 +/- 1.21646e-05
  return (TDataSet *)tableSet;// West = 5.53829 +/- 0.000543371 East = 5.54425 +/- 1.21677e-05
};
