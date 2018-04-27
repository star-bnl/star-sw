TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51554; // +/- 1.041e-05 cm/us All: East = -0.476945 +/- 0.00867856
  row.laserDriftVelocityWest	 =   5.51554; // +/- 1.041e-05 cm/us All: West = 0.324906 +/- 0.0019113
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51554 +/- 1.041e-05
  return (TDataSet *)tableSet;// West = 5.51535 +/- 1.06469e-05 East = 5.51974 +/- 4.96222e-05
};
