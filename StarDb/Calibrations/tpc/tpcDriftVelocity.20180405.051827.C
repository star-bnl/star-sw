TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5311; // +/- 1.13175e-05 cm/us All: East = -2.47191 +/- 0.00588695
  row.laserDriftVelocityWest	 =   5.5311; // +/- 1.13175e-05 cm/us All: West = -1.66401 +/- 0.00218499
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5311 +/- 1.13175e-05
  return (TDataSet *)tableSet;// West = 5.53057 +/- 1.21194e-05 East = 5.53473 +/- 3.16379e-05
};
