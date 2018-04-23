TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51835; // +/- 5.32092e-06 cm/us All: East = -0.398406 +/- 0.00287357
  row.laserDriftVelocityWest	 =   5.51835; // +/- 5.32092e-06 cm/us All: West = 0.256833 +/- 0.00101248
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51835 +/- 5.32092e-06
  return (TDataSet *)tableSet;// West = 5.51795 +/- 5.64043e-06 East = 5.52162 +/- 1.60373e-05
};
