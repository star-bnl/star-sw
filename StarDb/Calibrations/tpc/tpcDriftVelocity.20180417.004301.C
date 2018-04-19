TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55479; // +/- 1.88318e-05 cm/us All: East = 1.41145 +/- 3.64382
  row.laserDriftVelocityWest	 =   5.55479; // +/- 1.88318e-05 cm/us All: West = 0.179093 +/- 0.00335241
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55479 +/- 1.88318e-05
  return (TDataSet *)tableSet;// West = 5.55479 +/- 1.88321e-05 East = 5.55381 +/- 0.00356148
};
