TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55114; // +/- 5.98845e-05 cm/us All: East = -0.19011 +/- 0.0403459
  row.laserDriftVelocityWest	 =   5.55114; // +/- 5.98845e-05 cm/us All: West = 0.31094 +/- 0.0334939
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55114 +/- 5.98845e-05
  return (TDataSet *)tableSet;// West = 5.55023 +/- 7.51767e-05 East = 5.55272 +/- 9.906e-05
};
