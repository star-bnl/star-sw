TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5468; // +/- 5.11608e-06 cm/us All: East = 0.272786 +/- 0.00239156
  row.laserDriftVelocityWest	 =   5.5468; // +/- 5.11608e-06 cm/us All: West = 0.201055 +/- 0.00098085
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5468 +/- 5.11608e-06
  return (TDataSet *)tableSet;// West = 5.54684 +/- 5.55594e-06 East = 5.54658 +/- 1.31194e-05
};
