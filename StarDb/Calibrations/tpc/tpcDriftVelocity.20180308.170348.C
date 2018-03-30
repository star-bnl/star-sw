TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 67033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56507; // +/- 6.68592e-05 cm/us All: East = -4.34658 +/- 0.0849582
  row.laserDriftVelocityWest	 =   5.56507; // +/- 6.68592e-05 cm/us All: West = -4.05469 +/- 0.0293429
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56507 +/- 6.68592e-05
  return (TDataSet *)tableSet;// West = 5.56456 +/- 7.55289e-05 East = 5.56693 +/- 0.000143727
};
