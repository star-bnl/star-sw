TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86061
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52903; // +/- 6.97229e-06 cm/us All: East = -0.173041 +/- 1.1332
  row.laserDriftVelocityWest	 =   5.52903; // +/- 6.97229e-06 cm/us All: West = 0.164845 +/- 0.00124801
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52903 +/- 6.97229e-06
  return (TDataSet *)tableSet;// West = 5.52903 +/- 6.97229e-06 East = 5.53676 +/- 0.00635627
};
