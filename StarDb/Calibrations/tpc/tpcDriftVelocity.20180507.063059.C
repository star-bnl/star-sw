TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 127005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.57058; // +/- 7.84911e-06 cm/us All: East = -0.106729 +/- 0.00188287
  row.laserDriftVelocityWest	 =   5.57058; // +/- 7.84911e-06 cm/us All: West = 0.217852 +/- 0.00207877
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.57058 +/- 7.84911e-06
  return (TDataSet *)tableSet;// West = 5.56961 +/- 1.15536e-05 East = 5.57141 +/- 1.06965e-05
};
