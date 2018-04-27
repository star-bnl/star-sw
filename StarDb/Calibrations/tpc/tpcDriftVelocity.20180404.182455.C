TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52041; // +/- 1.02628e-05 cm/us All: East = -0.143981 +/- 0.0037827
  row.laserDriftVelocityWest	 =   5.52041; // +/- 1.02628e-05 cm/us All: West = 0.264499 +/- 0.00215425
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52041 +/- 1.02628e-05
  return (TDataSet *)tableSet;// West = 5.51986 +/- 1.18839e-05 East = 5.52203 +/- 2.03546e-05
};
