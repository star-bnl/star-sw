TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54685; // +/- 5.98062e-06 cm/us All: East = -0.0608966 +/- 0.00409682
  row.laserDriftVelocityWest	 =   5.54685; // +/- 5.98062e-06 cm/us All: West = 0.199451 +/- 0.0010931
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54685 +/- 5.98062e-06
  return (TDataSet *)tableSet;// West = 5.54674 +/- 6.21034e-06 East = 5.54816 +/- 2.21946e-05
};
