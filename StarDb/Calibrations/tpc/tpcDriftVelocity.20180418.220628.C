TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54431; // +/- 1.04694e-05 cm/us All: East = 1.41291 +/- 0.00471997
  row.laserDriftVelocityWest	 =   5.54431; // +/- 1.04694e-05 cm/us All: West = 2.26927 +/- 0.00138589
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54431 +/- 1.04694e-05
  return (TDataSet *)tableSet;// West = 5.54346 +/- 1.15189e-05 East = 5.54837 +/- 2.51041e-05
};
