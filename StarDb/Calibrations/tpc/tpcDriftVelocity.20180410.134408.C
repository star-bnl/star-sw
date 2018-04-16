TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54163; // +/- 1.76153e-05 cm/us All: East = -3.92986 +/- 0.00570605
  row.laserDriftVelocityWest	 =   5.54163; // +/- 1.76153e-05 cm/us All: West = -3.2312 +/- 0.00368418
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54163 +/- 1.76153e-05
  return (TDataSet *)tableSet;// West = 5.54055 +/- 2.08153e-05 East = 5.54436 +/- 3.30646e-05
};
