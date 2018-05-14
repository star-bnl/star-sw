TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54736; // +/- 9.01752e-06 cm/us All: East = -0.1071 +/- 0.00270326
  row.laserDriftVelocityWest	 =   5.54736; // +/- 9.01752e-06 cm/us All: West = 0.237969 +/- 0.00197706
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54736 +/- 9.01752e-06
  return (TDataSet *)tableSet;// West = 5.54674 +/- 1.1037e-05 East = 5.5486 +/- 1.56391e-05
};
