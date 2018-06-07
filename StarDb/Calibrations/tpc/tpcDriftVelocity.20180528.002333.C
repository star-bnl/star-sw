TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55021; // +/- 5.14835e-06 cm/us All: East = 0.0443341 +/- 0.0042705
  row.laserDriftVelocityWest	 =   5.55021; // +/- 5.14835e-06 cm/us All: West = 0.169239 +/- 0.000933764
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55021 +/- 5.14835e-06
  return (TDataSet *)tableSet;// West = 5.55018 +/- 5.27132e-06 East = 5.55077 +/- 2.39746e-05
};
