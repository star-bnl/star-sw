TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53175; // +/- 1.63954e-05 cm/us All: East = -0.356739 +/- 0.00459991
  row.laserDriftVelocityWest	 =   5.53175; // +/- 1.63954e-05 cm/us All: West = 0.380098 +/- 0.00389027
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53175 +/- 1.63954e-05
  return (TDataSet *)tableSet;// West = 5.53014 +/- 2.13524e-05 East = 5.53406 +/- 2.55925e-05
};
