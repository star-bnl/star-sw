TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55169; // +/- 5.07914e-06 cm/us All: East = 0.249108 +/- 0.00172493
  row.laserDriftVelocityWest	 =   5.55169; // +/- 5.07914e-06 cm/us All: West = 0.131476 +/- 0.00105426
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55169 +/- 5.07914e-06
  return (TDataSet *)tableSet;// West = 5.55187 +/- 5.99095e-06 East = 5.55122 +/- 9.57765e-06
};
