TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54769; // +/- 1.14533e-05 cm/us All: East = -0.742399 +/- 0.00682848
  row.laserDriftVelocityWest	 =   5.54769; // +/- 1.14533e-05 cm/us All: West = 0.263311 +/- 0.00213639
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54769 +/- 1.14533e-05
  return (TDataSet *)tableSet;// West = 5.54715 +/- 1.20504e-05 East = 5.55275 +/- 3.68436e-05
};
