TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 157007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55325; // +/- 5.06909e-06 cm/us All: East = 0.243056 +/- 0.00219383
  row.laserDriftVelocityWest	 =   5.55325; // +/- 5.06909e-06 cm/us All: West = 0.148874 +/- 0.000979209
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55325 +/- 5.06909e-06
  return (TDataSet *)tableSet;// West = 5.55334 +/- 5.56772e-06 East = 5.55283 +/- 1.22551e-05
};
