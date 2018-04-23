TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52997; // +/- 1.52015e-05 cm/us All: East = -0.582491 +/- 0.00786478
  row.laserDriftVelocityWest	 =   5.52997; // +/- 1.52015e-05 cm/us All: West = 0.275462 +/- 0.00297981
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52997 +/- 1.52015e-05
  return (TDataSet *)tableSet;// West = 5.52941 +/- 1.61805e-05 East = 5.53412 +/- 4.43765e-05
};
