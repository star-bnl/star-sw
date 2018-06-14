TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 158014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5516; // +/- 4.67896e-06 cm/us All: East = 0.216692 +/- 0.00175673
  row.laserDriftVelocityWest	 =   5.5516; // +/- 4.67896e-06 cm/us All: West = 0.15394 +/- 0.000950186
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5516 +/- 4.67896e-06
  return (TDataSet *)tableSet;// West = 5.55168 +/- 5.34688e-06 East = 5.55136 +/- 9.66782e-06
};
