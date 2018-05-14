TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54768; // +/- 1.06241e-05 cm/us All: East = -0.433651 +/- 0.0067829
  row.laserDriftVelocityWest	 =   5.54768; // +/- 1.06241e-05 cm/us All: West = 0.287462 +/- 0.00198776
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54768 +/- 1.06241e-05
  return (TDataSet *)tableSet;// West = 5.54735 +/- 1.1098e-05 East = 5.55135 +/- 3.67467e-05
};
