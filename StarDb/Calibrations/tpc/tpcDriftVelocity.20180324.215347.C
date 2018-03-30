TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53412; // +/- 0.000128415 cm/us All: East = 1.74712 +/- 14.2056
  row.laserDriftVelocityWest	 =   5.53412; // +/- 0.000128415 cm/us All: West = 1.46269 +/- 0.108237
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53412 +/- 0.000128415
  return (TDataSet *)tableSet;// West = 5.53412 +/- 0.000128415 East = 5.55932 +/- 0.0893069
};
