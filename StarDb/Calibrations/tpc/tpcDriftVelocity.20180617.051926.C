TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.546; // +/- 5.15723e-06 cm/us All: East = -0.295084 +/- 0.00337924
  row.laserDriftVelocityWest	 =   5.546; // +/- 5.15723e-06 cm/us All: West = -0.0353669 +/- 0.000948898
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.546 +/- 5.15723e-06
  return (TDataSet *)tableSet;// West = 5.5459 +/- 5.36665e-06 East = 5.54728 +/- 1.86434e-05
};
