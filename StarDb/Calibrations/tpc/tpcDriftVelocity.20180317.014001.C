TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54545; // +/- 1.10307e-05 cm/us All: East = -0.4372 +/- 3.85784
  row.laserDriftVelocityWest	 =   5.54545; // +/- 1.10307e-05 cm/us All: West = -0.626716 +/- 0.00198685
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54545 +/- 1.10307e-05
  return (TDataSet *)tableSet;// West = 5.54545 +/- 1.10308e-05 East = 5.54455 +/- 0.00227735
};
