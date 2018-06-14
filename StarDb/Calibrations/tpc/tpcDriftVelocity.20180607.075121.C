TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 158010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5521; // +/- 5.1959e-06 cm/us All: East = -0.108774 +/- 0.00191812
  row.laserDriftVelocityWest	 =   5.5521; // +/- 5.1959e-06 cm/us All: West = 0.166904 +/- 0.00104427
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5521 +/- 5.1959e-06
  return (TDataSet *)tableSet;// West = 5.55175 +/- 5.92447e-06 East = 5.55325 +/- 1.08147e-05
};
