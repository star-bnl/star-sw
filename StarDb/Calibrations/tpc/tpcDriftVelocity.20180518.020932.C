TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54149; // +/- 6.07802e-06 cm/us All: East = 0.43953 +/- 0.00724499
  row.laserDriftVelocityWest	 =   5.54149; // +/- 6.07802e-06 cm/us All: West = 0.139609 +/- 0.00109796
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54149 +/- 6.07802e-06
  return (TDataSet *)tableSet;// West = 5.54153 +/- 6.16898e-06 East = 5.53992 +/- 3.55256e-05
};
