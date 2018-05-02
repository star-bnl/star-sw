TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 121051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53228; // +/- 1.18017e-05 cm/us All: East = -0.770092 +/- 0.00473154
  row.laserDriftVelocityWest	 =   5.53228; // +/- 1.18017e-05 cm/us All: West = 0.196266 +/- 0.00155479
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53228 +/- 1.18017e-05
  return (TDataSet *)tableSet;// West = 5.53104 +/- 1.33953e-05 East = 5.53659 +/- 2.49475e-05
};
