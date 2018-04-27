TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 114027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54079; // +/- 7.33986e-06 cm/us All: East = -0.11878 +/- 0.00377863
  row.laserDriftVelocityWest	 =   5.54079; // +/- 7.33986e-06 cm/us All: West = 0.216174 +/- 0.00140497
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54079 +/- 7.33986e-06
  return (TDataSet *)tableSet;// West = 5.54057 +/- 7.8748e-06 East = 5.54225 +/- 2.02602e-05
};
