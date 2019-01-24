TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 23042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56463; // +/- 1.65039e-05 cm/us All: East = -0.480466 +/- 0.0090424
  row.laserDriftVelocityWest	 =   5.56463; // +/- 1.65039e-05 cm/us All: West = 0.257608 +/- 0.00323161
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56463 +/- 1.65039e-05
  return (TDataSet *)tableSet;// West = 5.56393 +/- 1.8145e-05 East = 5.56797 +/- 3.97126e-05
};
