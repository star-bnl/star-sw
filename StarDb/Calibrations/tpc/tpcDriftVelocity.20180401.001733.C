TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52826; // +/- 1.0496e-05 cm/us All: East = -2.8436 +/- 0.0064525
  row.laserDriftVelocityWest	 =   5.52826; // +/- 1.0496e-05 cm/us All: West = -2.09479 +/- 0.00195771
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52826 +/- 1.0496e-05
  return (TDataSet *)tableSet;// West = 5.52794 +/- 1.09253e-05 East = 5.53206 +/- 3.78152e-05
};
