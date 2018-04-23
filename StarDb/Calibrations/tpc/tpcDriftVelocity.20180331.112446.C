TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52746; // +/- 7.45921e-05 cm/us All: East = -1.13876 +/- 3.48554
  row.laserDriftVelocityWest	 =   5.52746; // +/- 7.45921e-05 cm/us All: West = 0.195839 +/- 0.0586914
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52746 +/- 7.45921e-05
  return (TDataSet *)tableSet;// West = 5.52737 +/- 7.51511e-05 East = 5.5334 +/- 0.000612661
};
