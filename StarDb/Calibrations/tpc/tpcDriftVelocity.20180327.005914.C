TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51892; // +/- 7.33671e-06 cm/us All: East = 0.270932 +/- 0.00801848
  row.laserDriftVelocityWest	 =   5.51892; // +/- 7.33671e-06 cm/us All: West = 0.160203 +/- 0.00134234
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51892 +/- 7.33671e-06
  return (TDataSet *)tableSet;// West = 5.51896 +/- 7.43503e-06 East = 5.51751 +/- 4.52618e-05
};
