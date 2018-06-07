TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55037; // +/- 5.81799e-06 cm/us All: East = -0.0652435 +/- 0.00397945
  row.laserDriftVelocityWest	 =   5.55037; // +/- 5.81799e-06 cm/us All: West = 0.165859 +/- 0.00105797
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55037 +/- 5.81799e-06
  return (TDataSet *)tableSet;// West = 5.55029 +/- 6.01639e-06 East = 5.5516 +/- 2.28436e-05
};
