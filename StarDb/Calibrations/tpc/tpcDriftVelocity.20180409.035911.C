TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55076; // +/- 4.00483e-05 cm/us All: East = -5.30496 +/- 0.0194412
  row.laserDriftVelocityWest	 =   5.55076; // +/- 4.00483e-05 cm/us All: West = -4.93706 +/- 0.0135035
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55076 +/- 4.00483e-05
  return (TDataSet *)tableSet;// West = 5.54982 +/- 5.60299e-05 East = 5.55174 +/- 5.7264e-05
};
