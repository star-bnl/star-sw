TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 144032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55085; // +/- 0.000190494 cm/us All: East = 0.0497357 +/- 0.325614
  row.laserDriftVelocityWest	 =   5.55085; // +/- 0.000190494 cm/us All: West = 0.213159 +/- 0.099688
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55085 +/- 0.000190494
  return (TDataSet *)tableSet;// West = 5.55071 +/- 0.000201444 East = 5.55198 +/- 0.000585753
};
