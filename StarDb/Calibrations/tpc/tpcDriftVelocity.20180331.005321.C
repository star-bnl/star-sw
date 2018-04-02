TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52629; // +/- 9.93116e-06 cm/us All: East = -2.56873 +/- 0.0077827
  row.laserDriftVelocityWest	 =   5.52629; // +/- 9.93116e-06 cm/us All: West = -1.75159 +/- 0.00183273
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52629 +/- 9.93116e-06
  return (TDataSet *)tableSet;// West = 5.52604 +/- 1.02215e-05 East = 5.53053 +/- 4.19657e-05
};
