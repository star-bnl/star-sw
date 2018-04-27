TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5174; // +/- 8.06944e-06 cm/us All: East = 0.321118 +/- 0.00691803
  row.laserDriftVelocityWest	 =   5.5174; // +/- 8.06944e-06 cm/us All: West = 0.265159 +/- 0.00148584
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5174 +/- 8.06944e-06
  return (TDataSet *)tableSet;// West = 5.51741 +/- 8.25073e-06 East = 5.51713 +/- 3.87071e-05
};
