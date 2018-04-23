TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51861; // +/- 1.17981e-05 cm/us All: East = -0.0942881 +/- 0.00640242
  row.laserDriftVelocityWest	 =   5.51861; // +/- 1.17981e-05 cm/us All: West = 0.20971 +/- 0.00234483
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51861 +/- 1.17981e-05
  return (TDataSet *)tableSet;// West = 5.51843 +/- 1.26292e-05 East = 5.51982 +/- 3.30699e-05
};
