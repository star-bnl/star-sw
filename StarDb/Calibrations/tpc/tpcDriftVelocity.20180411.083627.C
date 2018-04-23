TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 101011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55193; // +/- 1.05652e-05 cm/us All: East = -0.380526 +/- 0.35885
  row.laserDriftVelocityWest	 =   5.55193; // +/- 1.05652e-05 cm/us All: West = 0.190142 +/- 0.00187354
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55193 +/- 1.05652e-05
  return (TDataSet *)tableSet;// West = 5.55189 +/- 1.06275e-05 East = 5.55473 +/- 9.77147e-05
};
