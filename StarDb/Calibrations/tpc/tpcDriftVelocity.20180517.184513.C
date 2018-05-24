TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53778; // +/- 5.58365e-05 cm/us All: East = 0.555781 +/- 0.309197
  row.laserDriftVelocityWest	 =   5.53778; // +/- 5.58365e-05 cm/us All: West = 0.060475 +/- 0.0115358
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53778 +/- 5.58365e-05
  return (TDataSet *)tableSet;// West = 5.53679 +/- 6.72387e-05 East = 5.54 +/- 0.000100221
};
