TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 73033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55398; // +/- 1.0027e-05 cm/us All: East = -0.0374469 +/- 0.00700656
  row.laserDriftVelocityWest	 =   5.55398; // +/- 1.0027e-05 cm/us All: West = 0.109281 +/- 0.00185714
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55398 +/- 1.0027e-05
  return (TDataSet *)tableSet;// West = 5.55392 +/- 1.03675e-05 East = 5.5548 +/- 3.94526e-05
};
