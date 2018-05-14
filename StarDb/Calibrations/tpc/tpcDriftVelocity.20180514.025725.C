TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133059
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54735; // +/- 1.09241e-05 cm/us All: East = -0.160888 +/- 0.00371175
  row.laserDriftVelocityWest	 =   5.54735; // +/- 1.09241e-05 cm/us All: West = 0.252497 +/- 0.00230356
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54735 +/- 1.09241e-05
  return (TDataSet *)tableSet;// West = 5.54675 +/- 1.27738e-05 East = 5.54897 +/- 2.10769e-05
};
