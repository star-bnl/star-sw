TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5555; // +/- 1.33344e-05 cm/us All: East = -0.20964 +/- 0.00418664
  row.laserDriftVelocityWest	 =   5.5555; // +/- 1.33344e-05 cm/us All: West = 0.179287 +/- 0.00295065
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5555 +/- 1.33344e-05
  return (TDataSet *)tableSet;// West = 5.55476 +/- 1.64438e-05 East = 5.5569 +/- 2.27869e-05
};
