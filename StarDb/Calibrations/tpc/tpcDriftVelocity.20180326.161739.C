TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52914; // +/- 8.55383e-06 cm/us All: East = -0.332856 +/- 0.0031096
  row.laserDriftVelocityWest	 =   5.52914; // +/- 8.55383e-06 cm/us All: West = 0.174434 +/- 0.0021421
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52914 +/- 8.55383e-06
  return (TDataSet *)tableSet;// West = 5.52825 +/- 1.0471e-05 East = 5.53093 +/- 1.48306e-05
};
