TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 114040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54081; // +/- 7.517e-06 cm/us All: East = 0.201277 +/- 0.00225209
  row.laserDriftVelocityWest	 =   5.54081; // +/- 7.517e-06 cm/us All: West = 0.179763 +/- 0.00112311
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54081 +/- 7.517e-06
  return (TDataSet *)tableSet;// West = 5.54058 +/- 9.50395e-06 East = 5.54119 +/- 1.22847e-05
};
