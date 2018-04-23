TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 101001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5537; // +/- 1.03273e-05 cm/us All: East = 0.120023 +/- 0.00190444
  row.laserDriftVelocityWest	 =   5.5537; // +/- 1.03273e-05 cm/us All: West = 0.668175 +/- 0.00728904
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5537 +/- 1.03273e-05
  return (TDataSet *)tableSet;// West = 5.55085 +/- 3.48718e-05 East = 5.55397 +/- 1.08123e-05
};
