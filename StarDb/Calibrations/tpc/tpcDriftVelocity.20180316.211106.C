TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54334; // +/- 1.00408e-05 cm/us All: East = 0.483204 +/- 1.3488
  row.laserDriftVelocityWest	 =   5.54334; // +/- 1.00408e-05 cm/us All: West = -0.243516 +/- 0.00181241
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54334 +/- 1.00408e-05
  return (TDataSet *)tableSet;// West = 5.54334 +/- 1.00414e-05 East = 5.54349 +/- 0.000916826
};
