TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54935; // +/- 1.30652e-05 cm/us All: East = 0.025833 +/- 0.00328253
  row.laserDriftVelocityWest	 =   5.54935; // +/- 1.30652e-05 cm/us All: West = 0.304012 +/- 0.00326681
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54935 +/- 1.30652e-05
  return (TDataSet *)tableSet;// West = 5.54864 +/- 1.81906e-05 East = 5.55011 +/- 1.87774e-05
};
