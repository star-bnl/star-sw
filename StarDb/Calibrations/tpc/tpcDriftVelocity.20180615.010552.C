TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54697; // +/- 7.51913e-06 cm/us All: East = 0.306493 +/- 14.8951
  row.laserDriftVelocityWest	 =   5.54697; // +/- 7.51913e-06 cm/us All: West = 0.18862 +/- 0.00133493
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54697 +/- 7.51913e-06
  return (TDataSet *)tableSet;// West = 5.54697 +/- 7.51913e-06 East = 5.54677 +/- 0.00737118
};
