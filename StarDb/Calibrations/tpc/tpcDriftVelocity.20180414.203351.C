TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54351; // +/- 1.7948e-05 cm/us All: East = 0.0681327 +/- 0.00509408
  row.laserDriftVelocityWest	 =   5.54351; // +/- 1.7948e-05 cm/us All: West = 0.31811 +/- 0.00528202
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54351 +/- 1.7948e-05
  return (TDataSet *)tableSet;// West = 5.54274 +/- 2.63978e-05 East = 5.54417 +/- 2.44757e-05
};
