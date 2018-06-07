TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55143; // +/- 5.15195e-06 cm/us All: East = 0.243185 +/- 0.00178327
  row.laserDriftVelocityWest	 =   5.55143; // +/- 5.15195e-06 cm/us All: West = 0.174516 +/- 0.00105908
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55143 +/- 5.15195e-06
  return (TDataSet *)tableSet;// West = 5.55153 +/- 6.02944e-06 East = 5.55115 +/- 9.91699e-06
};
