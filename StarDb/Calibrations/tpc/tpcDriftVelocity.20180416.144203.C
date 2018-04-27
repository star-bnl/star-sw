TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54582; // +/- 1.0277e-05 cm/us All: East = -0.0483953 +/- 0.00246042
  row.laserDriftVelocityWest	 =   5.54582; // +/- 1.0277e-05 cm/us All: West = 0.165064 +/- 0.00277387
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54582 +/- 1.0277e-05
  return (TDataSet *)tableSet;// West = 5.54514 +/- 1.53999e-05 East = 5.54636 +/- 1.37993e-05
};
