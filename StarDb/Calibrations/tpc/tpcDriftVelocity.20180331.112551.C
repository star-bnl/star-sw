TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52729; // +/- 1.09945e-05 cm/us All: East = -0.73668 +/- 0.0124113
  row.laserDriftVelocityWest	 =   5.52729; // +/- 1.09945e-05 cm/us All: West = 0.211944 +/- 0.00198213
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52729 +/- 1.09945e-05
  return (TDataSet *)tableSet;// West = 5.52718 +/- 1.11194e-05 East = 5.53235 +/- 7.35512e-05
};
