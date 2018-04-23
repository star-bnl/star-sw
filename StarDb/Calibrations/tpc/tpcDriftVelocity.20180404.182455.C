TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52044; // +/- 1.02931e-05 cm/us All: East = -0.137491 +/- 0.00376218
  row.laserDriftVelocityWest	 =   5.52044; // +/- 1.02931e-05 cm/us All: West = 0.304365 +/- 0.00215187
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52044 +/- 1.02931e-05
  return (TDataSet *)tableSet;// West = 5.51986 +/- 1.19155e-05 East = 5.52212 +/- 2.04326e-05
};
