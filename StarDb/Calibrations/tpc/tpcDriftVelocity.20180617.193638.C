TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.546; // +/- 6.58401e-06 cm/us All: East = 0.216468 +/- 0.00230778
  row.laserDriftVelocityWest	 =   5.546; // +/- 6.58401e-06 cm/us All: West = 0.181015 +/- 0.00134101
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.546 +/- 6.58401e-06
  return (TDataSet *)tableSet;// West = 5.54606 +/- 7.62857e-06 East = 5.54583 +/- 1.30356e-05
};
