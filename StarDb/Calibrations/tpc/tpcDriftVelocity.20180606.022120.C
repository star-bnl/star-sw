TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55319; // +/- 4.1425e-05 cm/us All: East = -0.041247 +/- 0.0305655
  row.laserDriftVelocityWest	 =   5.55319; // +/- 4.1425e-05 cm/us All: West = 0.0724348 +/- 0.00887472
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55319 +/- 4.1425e-05
  return (TDataSet *)tableSet;// West = 5.55266 +/- 5.7437e-05 East = 5.55377 +/- 5.98023e-05
};
