TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5543; // +/- 0.000113401 cm/us All: East = -0.782284 +/- 7.48056
  row.laserDriftVelocityWest	 =   5.5543; // +/- 0.000113401 cm/us All: West = 0.558687 +/- 0.164292
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5543 +/- 0.000113401
  return (TDataSet *)tableSet;// West = 5.55311 +/- 0.00012739 East = 5.55882 +/- 0.000248905
};
