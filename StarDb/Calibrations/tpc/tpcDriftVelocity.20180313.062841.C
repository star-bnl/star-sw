TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54209; // +/- 2.88793e-05 cm/us All: East = -0.0126397 +/- 0.0660101
  row.laserDriftVelocityWest	 =   5.54209; // +/- 2.88793e-05 cm/us All: West = -0.0257386 +/- 0.00566507
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54209 +/- 2.88793e-05
  return (TDataSet *)tableSet;// West = 5.54201 +/- 2.9622e-05 East = 5.54365 +/- 0.000129782
};
