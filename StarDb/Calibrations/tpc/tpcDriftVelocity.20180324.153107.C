TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.534; // +/- 9.34122e-06 cm/us All: East = 1.46653 +/- 15.367
  row.laserDriftVelocityWest	 =   5.534; // +/- 9.34122e-06 cm/us All: West = 1.44133 +/- 0.0016732
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.534 +/- 9.34122e-06
  return (TDataSet *)tableSet;// West = 5.534 +/- 9.34122e-06 East = 5.54241 +/- 0.0274994
};
