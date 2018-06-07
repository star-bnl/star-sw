TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55277; // +/- 6.0351e-06 cm/us All: East = 0.27576 +/- 0.00246763
  row.laserDriftVelocityWest	 =   5.55277; // +/- 6.0351e-06 cm/us All: West = 0.147251 +/- 0.00117949
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55277 +/- 6.0351e-06
  return (TDataSet *)tableSet;// West = 5.55291 +/- 6.7006e-06 East = 5.55218 +/- 1.38903e-05
};
