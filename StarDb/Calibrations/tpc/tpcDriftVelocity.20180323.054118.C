TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54072; // +/- 1.48621e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54072; // +/- 1.48621e-05 cm/us All: West = 0.164823 +/- 0.00265135
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54072 +/- 1.48621e-05
  return (TDataSet *)tableSet;// West = 5.54072 +/- 1.48621e-05 East = -999 +/- 999
};
