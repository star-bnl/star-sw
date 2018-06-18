TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 166002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.547; // +/- 8.04243e-06 cm/us All: East = -0.561805 +/- 0.00554555
  row.laserDriftVelocityWest	 =   5.547; // +/- 8.04243e-06 cm/us All: West = 0.301854 +/- 0.00148386
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.547 +/- 8.04243e-06
  return (TDataSet *)tableSet;// West = 5.54669 +/- 8.31037e-06 East = 5.55153 +/- 3.19296e-05
};
