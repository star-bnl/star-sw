TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 66008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52949; // +/- 1.19658e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.52949; // +/- 1.19658e-05 cm/us All: West = 0.211544 +/- 0.00213858
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52949 +/- 1.19658e-05
  return (TDataSet *)tableSet;// West = 5.52949 +/- 1.19658e-05 East = -999 +/- 999
};
