TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55347; // +/- 1.83625e-05 cm/us All: East = 0.0589726 +/- 0.00531924
  row.laserDriftVelocityWest	 =   5.55347; // +/- 1.83625e-05 cm/us All: West = 0.323974 +/- 0.00521798
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55347 +/- 1.83625e-05
  return (TDataSet *)tableSet;// West = 5.55272 +/- 2.63618e-05 East = 5.55418 +/- 2.55923e-05
};
