TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54404; // +/- 7.46362e-06 cm/us All: East = 0.0705011 +/- 0.00170639
  row.laserDriftVelocityWest	 =   5.54404; // +/- 7.46362e-06 cm/us All: West = 0.257033 +/- 0.00214287
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54404 +/- 7.46362e-06
  return (TDataSet *)tableSet;// West = 5.54341 +/- 1.1965e-05 East = 5.54444 +/- 9.54921e-06
};
