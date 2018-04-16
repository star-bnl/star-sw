TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55408; // +/- 8.56506e-06 cm/us All: East = -5.76018 +/- 0.00190986
  row.laserDriftVelocityWest	 =   5.55408; // +/- 8.56506e-06 cm/us All: West = -5.49996 +/- 0.00252298
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55408 +/- 8.56506e-06
  return (TDataSet *)tableSet;// West = 5.55316 +/- 1.42071e-05 East = 5.5546 +/- 1.07353e-05
};
