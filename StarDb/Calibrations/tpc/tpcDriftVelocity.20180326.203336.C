TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52887; // +/- 7.80319e-06 cm/us All: East = 1.90282 +/- 0.00591953
  row.laserDriftVelocityWest	 =   5.52887; // +/- 7.80319e-06 cm/us All: West = 2.39485 +/- 0.00144285
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52887 +/- 7.80319e-06
  return (TDataSet *)tableSet;// West = 5.52873 +/- 8.02369e-06 East = 5.53138 +/- 3.35149e-05
};
