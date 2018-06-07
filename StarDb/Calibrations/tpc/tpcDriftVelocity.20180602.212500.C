TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55243; // +/- 4.77635e-06 cm/us All: East = 0.224779 +/- 0.00190786
  row.laserDriftVelocityWest	 =   5.55243; // +/- 4.77635e-06 cm/us All: West = 0.0466387 +/- 0.000947094
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55243 +/- 4.77635e-06
  return (TDataSet *)tableSet;// West = 5.55262 +/- 5.33741e-06 East = 5.55164 +/- 1.0702e-05
};
