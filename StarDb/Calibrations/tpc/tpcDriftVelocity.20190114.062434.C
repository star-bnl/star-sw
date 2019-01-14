TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 14003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54834; // +/- 1.45919e-05 cm/us All: East = -1.34063 +/- 0.00475016
  row.laserDriftVelocityWest	 =   5.54834; // +/- 1.45919e-05 cm/us All: West = -0.821862 +/- 0.00306117
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54834 +/- 1.45919e-05
  return (TDataSet *)tableSet;// West = 5.54752 +/- 1.71961e-05 East = 5.55045 +/- 2.75786e-05
};
