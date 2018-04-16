TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54679; // +/- 1.36691e-05 cm/us All: East = -5.52341 +/- 11.1432
  row.laserDriftVelocityWest	 =   5.54679; // +/- 1.36691e-05 cm/us All: West = -4.35656 +/- 0.00243672
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54679 +/- 1.36691e-05
  return (TDataSet *)tableSet;// West = 5.54679 +/- 1.36691e-05 East = 5.54837 +/- 0.0440431
};
