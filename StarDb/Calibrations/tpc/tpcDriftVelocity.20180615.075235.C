TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 166007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54738; // +/- 8.09757e-06 cm/us All: East = -0.559452 +/- 0.00345787
  row.laserDriftVelocityWest	 =   5.54738; // +/- 8.09757e-06 cm/us All: West = 0.338417 +/- 0.00160958
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54738 +/- 8.09757e-06
  return (TDataSet *)tableSet;// West = 5.5465 +/- 8.91709e-06 East = 5.5515 +/- 1.93369e-05
};
