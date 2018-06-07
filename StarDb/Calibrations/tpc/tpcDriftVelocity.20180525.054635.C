TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55028; // +/- 0.000180154 cm/us All: East = -3.97464 +/- 6.97972
  row.laserDriftVelocityWest	 =   5.55028; // +/- 0.000180154 cm/us All: West = 0.262167 +/- 0.153946
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55028 +/- 0.000180154
  return (TDataSet *)tableSet;// West = 5.55028 +/- 0.000180157 East = 5.61167 +/- 0.0294877
};
