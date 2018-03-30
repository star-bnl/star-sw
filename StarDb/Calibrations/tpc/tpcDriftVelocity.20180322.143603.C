TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54686; // +/- 1.2536e-05 cm/us All: East = -0.715111 +/- 0.0403973
  row.laserDriftVelocityWest	 =   5.54686; // +/- 1.2536e-05 cm/us All: West = -0.881374 +/- 0.00223869
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54686 +/- 1.2536e-05
  return (TDataSet *)tableSet;// West = 5.54688 +/- 1.26332e-05 East = 5.54582 +/- 0.000101241
};
