TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55326; // +/- 4.33855e-06 cm/us All: East = 0.258049 +/- 0.00140655
  row.laserDriftVelocityWest	 =   5.55326; // +/- 4.33855e-06 cm/us All: West = 0.119371 +/- 0.000927
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55326 +/- 4.33855e-06
  return (TDataSet *)tableSet;// West = 5.55349 +/- 5.22179e-06 East = 5.55274 +/- 7.7963e-06
};
