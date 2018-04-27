TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51856; // +/- 1.16626e-05 cm/us All: East = -0.0852323 +/- 0.00649923
  row.laserDriftVelocityWest	 =   5.51856; // +/- 1.16626e-05 cm/us All: West = 0.210953 +/- 0.00229597
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51856 +/- 1.16626e-05
  return (TDataSet *)tableSet;// West = 5.5184 +/- 1.24622e-05 East = 5.51974 +/- 3.30927e-05
};
