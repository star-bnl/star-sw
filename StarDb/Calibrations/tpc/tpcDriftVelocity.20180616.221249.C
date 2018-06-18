TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54681; // +/- 5.99569e-06 cm/us All: East = -0.00736942 +/- 0.00396583
  row.laserDriftVelocityWest	 =   5.54681; // +/- 5.99569e-06 cm/us All: West = 0.204708 +/- 0.00110887
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54681 +/- 5.99569e-06
  return (TDataSet *)tableSet;// West = 5.54672 +/- 6.24939e-06 East = 5.54789 +/- 2.12584e-05
};
