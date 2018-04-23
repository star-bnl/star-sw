TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52975; // +/- 9.57164e-06 cm/us All: East = -0.491279 +/- 0.00583135
  row.laserDriftVelocityWest	 =   5.52975; // +/- 9.57164e-06 cm/us All: West = 0.221355 +/- 0.00179155
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52975 +/- 9.57164e-06
  return (TDataSet *)tableSet;// West = 5.52943 +/- 9.97669e-06 East = 5.53337 +/- 3.39363e-05
};
