TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74100
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53205; // +/- 1.46563e-05 cm/us All: East = 1.91047 +/- 0.0161599
  row.laserDriftVelocityWest	 =   5.53205; // +/- 1.46563e-05 cm/us All: West = 1.77611 +/- 0.00277402
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53205 +/- 1.46563e-05
  return (TDataSet *)tableSet;// West = 5.53208 +/- 1.49951e-05 East = 5.53139 +/- 6.93325e-05
};
