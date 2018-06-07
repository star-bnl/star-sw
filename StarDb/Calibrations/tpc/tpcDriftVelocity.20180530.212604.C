TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 150053
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54939; // +/- 1.95573e-05 cm/us All: East = -0.21488 +/- 0.00620818
  row.laserDriftVelocityWest	 =   5.54939; // +/- 1.95573e-05 cm/us All: West = 0.177673 +/- 0.00419746
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54939 +/- 1.95573e-05
  return (TDataSet *)tableSet;// West = 5.54895 +/- 2.27487e-05 East = 5.55063 +/- 3.82887e-05
};
