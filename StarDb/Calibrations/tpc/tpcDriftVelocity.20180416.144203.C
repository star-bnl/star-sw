TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55574; // +/- 1.04877e-05 cm/us All: East = -0.0993976 +/- 0.00253908
  row.laserDriftVelocityWest	 =   5.55574; // +/- 1.04877e-05 cm/us All: West = 0.131492 +/- 0.0027592
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55574 +/- 1.04877e-05
  return (TDataSet *)tableSet;// West = 5.55504 +/- 1.54067e-05 East = 5.55635 +/- 1.43168e-05
};
