TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53742; // +/- 5.78509e-06 cm/us All: East = 0.104659 +/- 0.00925648
  row.laserDriftVelocityWest	 =   5.53742; // +/- 5.78509e-06 cm/us All: West = -0.0185621 +/- 0.00103439
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53742 +/- 5.78509e-06
  return (TDataSet *)tableSet;// West = 5.53744 +/- 5.82792e-06 East = 5.53652 +/- 4.7806e-05
};
