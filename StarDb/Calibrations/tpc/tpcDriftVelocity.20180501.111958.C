TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 121015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53249; // +/- 5.97506e-06 cm/us All: East = -0.0231731 +/- 0.00182971
  row.laserDriftVelocityWest	 =   5.53249; // +/- 5.97506e-06 cm/us All: West = 0.134054 +/- 0.00130827
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53249 +/- 5.97506e-06
  return (TDataSet *)tableSet;// West = 5.53219 +/- 7.36143e-06 East = 5.53307 +/- 1.02293e-05
};
