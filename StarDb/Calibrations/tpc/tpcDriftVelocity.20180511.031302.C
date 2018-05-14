TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130083
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55079; // +/- 1.377e-05 cm/us All: East = -0.167292 +/- 0.00657572
  row.laserDriftVelocityWest	 =   5.55079; // +/- 1.377e-05 cm/us All: West = 0.147785 +/- 0.00275663
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55079 +/- 1.377e-05
  return (TDataSet *)tableSet;// West = 5.55057 +/- 1.50544e-05 East = 5.5519 +/- 3.40691e-05
};
