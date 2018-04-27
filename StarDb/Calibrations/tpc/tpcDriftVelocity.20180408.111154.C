TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5381; // +/- 1.33329e-05 cm/us All: East = -0.706216 +/- 0.00977272
  row.laserDriftVelocityWest	 =   5.5381; // +/- 1.33329e-05 cm/us All: West = 0.241879 +/- 0.00248972
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5381 +/- 1.33329e-05
  return (TDataSet *)tableSet;// West = 5.5378 +/- 1.3711e-05 East = 5.54315 +/- 5.71636e-05
};
