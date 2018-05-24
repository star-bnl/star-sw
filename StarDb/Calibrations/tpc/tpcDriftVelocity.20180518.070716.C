TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54319; // +/- 9.07754e-05 cm/us All: East = 1.664 +/- 5.54598
  row.laserDriftVelocityWest	 =   5.54319; // +/- 9.07754e-05 cm/us All: West = -0.234218 +/- 0.0203378
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54319 +/- 9.07754e-05
  return (TDataSet *)tableSet;// West = 5.5432 +/- 9.09797e-05 East = 5.54224 +/- 0.00135528
};
