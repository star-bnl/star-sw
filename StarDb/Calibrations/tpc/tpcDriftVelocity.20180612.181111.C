TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54842; // +/- 6.27562e-06 cm/us All: East = -0.428427 +/- 0.00299387
  row.laserDriftVelocityWest	 =   5.54842; // +/- 6.27562e-06 cm/us All: West = 0.151305 +/- 0.00120077
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54842 +/- 6.27562e-06
  return (TDataSet *)tableSet;// West = 5.548 +/- 6.75332e-06 East = 5.55105 +/- 1.69881e-05
};
