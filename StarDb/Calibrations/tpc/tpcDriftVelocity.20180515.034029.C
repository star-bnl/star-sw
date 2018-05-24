TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54544; // +/- 7.23361e-06 cm/us All: East = 0.243368 +/- 0.00179219
  row.laserDriftVelocityWest	 =   5.54544; // +/- 7.23361e-06 cm/us All: West = 0.152825 +/- 0.00101041
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54544 +/- 7.23361e-06
  return (TDataSet *)tableSet;// West = 5.54521 +/- 1.04857e-05 East = 5.54566 +/- 9.99186e-06
};
