TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162022
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54726; // +/- 1.06211e-05 cm/us All: East = -0.0143034 +/- 0.00415187
  row.laserDriftVelocityWest	 =   5.54726; // +/- 1.06211e-05 cm/us All: West = 0.262721 +/- 0.00216106
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54726 +/- 1.06211e-05
  return (TDataSet *)tableSet;// West = 5.54693 +/- 1.20054e-05 East = 5.54844 +/- 2.27836e-05
};
