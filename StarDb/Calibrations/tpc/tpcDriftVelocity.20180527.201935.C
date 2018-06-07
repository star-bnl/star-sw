TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55004; // +/- 4.70151e-06 cm/us All: East = 0.134355 +/- 0.00172791
  row.laserDriftVelocityWest	 =   5.55004; // +/- 4.70151e-06 cm/us All: West = 0.122774 +/- 0.000940566
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55004 +/- 4.70151e-06
  return (TDataSet *)tableSet;// West = 5.55005 +/- 5.36267e-06 East = 5.55 +/- 9.77406e-06
};
