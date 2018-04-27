TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54301; // +/- 1.07728e-05 cm/us All: East = -0.197349 +/- 0.00393748
  row.laserDriftVelocityWest	 =   5.54301; // +/- 1.07728e-05 cm/us All: West = 0.259224 +/- 0.00138683
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54301 +/- 1.07728e-05
  return (TDataSet *)tableSet;// West = 5.5423 +/- 1.24159e-05 East = 5.54518 +/- 2.16687e-05
};
