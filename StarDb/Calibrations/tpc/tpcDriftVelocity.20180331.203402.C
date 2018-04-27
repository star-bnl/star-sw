TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5181; // +/- 9.38204e-06 cm/us All: East = -0.311625 +/- 0.00393544
  row.laserDriftVelocityWest	 =   5.5181; // +/- 9.38204e-06 cm/us All: West = 0.370569 +/- 0.00184922
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5181 +/- 9.38204e-06
  return (TDataSet *)tableSet;// West = 5.51743 +/- 1.03551e-05 East = 5.5212 +/- 2.21686e-05
};
