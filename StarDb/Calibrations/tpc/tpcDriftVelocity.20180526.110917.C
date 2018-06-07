TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 146019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55152; // +/- 0.000127172 cm/us All: East = -0.116963 +/- 0.511921
  row.laserDriftVelocityWest	 =   5.55152; // +/- 0.000127172 cm/us All: West = 0.0833392 +/- 0.0679713
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55152 +/- 0.000127172
  return (TDataSet *)tableSet;// West = 5.55127 +/- 0.000136542 East = 5.55311 +/- 0.000349314
};
