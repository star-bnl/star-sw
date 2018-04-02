TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52823; // +/- 9.3722e-06 cm/us All: East = -2.75218 +/- 0.00373353
  row.laserDriftVelocityWest	 =   5.52823; // +/- 9.3722e-06 cm/us All: West = -1.99854 +/- 0.00186098
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52823 +/- 9.3722e-06
  return (TDataSet *)tableSet;// West = 5.5274 +/- 1.04559e-05 East = 5.53159 +/- 2.11404e-05
};
