TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54329; // +/- 6.98658e-06 cm/us All: East = 0.0788885 +/- 0.00171897
  row.laserDriftVelocityWest	 =   5.54329; // +/- 6.98658e-06 cm/us All: West = 0.361985 +/- 0.00181605
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54329 +/- 6.98658e-06
  return (TDataSet *)tableSet;// West = 5.54246 +/- 1.01064e-05 East = 5.54404 +/- 9.66913e-06
};
