TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54766; // +/- 9.9538e-06 cm/us All: East = -0.346202 +/- 0.00352275
  row.laserDriftVelocityWest	 =   5.54766; // +/- 9.9538e-06 cm/us All: West = 0.265612 +/- 0.00205331
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54766 +/- 9.9538e-06
  return (TDataSet *)tableSet;// West = 5.5468 +/- 1.15019e-05 East = 5.5502 +/- 1.98651e-05
};
