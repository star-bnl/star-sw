TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55022; // +/- 1.07e-05 cm/us All: East = -5.13256 +/- 0.00259131
  row.laserDriftVelocityWest	 =   5.55022; // +/- 1.07e-05 cm/us All: West = -4.79724 +/- 0.00278965
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55022 +/- 1.07e-05
  return (TDataSet *)tableSet;// West = 5.54923 +/- 1.56086e-05 East = 5.55111 +/- 1.46969e-05
};
