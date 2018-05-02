TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55331; // +/- 1.2658e-05 cm/us All: East = 2.64736 +/- 7.99942
  row.laserDriftVelocityWest	 =   5.55331; // +/- 1.2658e-05 cm/us All: West = 0.172819 +/- 0.00224967
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55331 +/- 1.2658e-05
  return (TDataSet *)tableSet;// West = 5.55331 +/- 1.26581e-05 East = 5.54429 +/- 0.00280532
};
