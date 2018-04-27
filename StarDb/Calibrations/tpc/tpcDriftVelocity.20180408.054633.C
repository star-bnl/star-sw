TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53683; // +/- 1.36138e-05 cm/us All: East = -0.524449 +/- 1.54204
  row.laserDriftVelocityWest	 =   5.53683; // +/- 1.36138e-05 cm/us All: West = 0.18857 +/- 0.0024442
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53683 +/- 1.36138e-05
  return (TDataSet *)tableSet;// West = 5.53683 +/- 1.36139e-05 East = 5.54073 +/- 0.00356492
};
