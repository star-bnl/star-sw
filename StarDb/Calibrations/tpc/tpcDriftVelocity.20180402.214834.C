TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92084
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52948; // +/- 2.37231e-05 cm/us All: East = -0.794463 +/- 0.0152592
  row.laserDriftVelocityWest	 =   5.52948; // +/- 2.37231e-05 cm/us All: West = 0.339882 +/- 0.00472156
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52948 +/- 2.37231e-05
  return (TDataSet *)tableSet;// West = 5.52872 +/- 2.53399e-05 East = 5.53492 +/- 6.7496e-05
};
