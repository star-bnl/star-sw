TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 17012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5571; // +/- 8.84723e-06 cm/us All: East = 0.219158 +/- 0.00321995
  row.laserDriftVelocityWest	 =   5.5571; // +/- 8.84723e-06 cm/us All: West = 0.431994 +/- 0.00182535
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5571 +/- 8.84723e-06
  return (TDataSet *)tableSet;// West = 5.55683 +/- 1.021e-05 East = 5.55792 +/- 1.7725e-05
};
