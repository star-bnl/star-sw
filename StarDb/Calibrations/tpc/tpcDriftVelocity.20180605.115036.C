TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55498; // +/- 1.15302e-05 cm/us All: East = 0.0491371 +/- 0.00620114
  row.laserDriftVelocityWest	 =   5.55498; // +/- 1.15302e-05 cm/us All: West = 0.390033 +/- 0.00210942
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55498 +/- 1.15302e-05
  return (TDataSet *)tableSet;// West = 5.55478 +/- 1.22033e-05 East = 5.55663 +/- 3.52053e-05
};
