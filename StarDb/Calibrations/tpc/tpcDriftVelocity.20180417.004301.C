TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55481; // +/- 1.87388e-05 cm/us All: East = 2.19225 +/- 9.56041
  row.laserDriftVelocityWest	 =   5.55481; // +/- 1.87388e-05 cm/us All: West = 0.186596 +/- 0.00338745
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55481 +/- 1.87388e-05
  return (TDataSet *)tableSet;// West = 5.55481 +/- 1.8739e-05 East = 5.54987 +/- 0.00440073
};
