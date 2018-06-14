TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.548; // +/- 8.07358e-06 cm/us All: East = 0.0453046 +/- 0.00366479
  row.laserDriftVelocityWest	 =   5.548; // +/- 8.07358e-06 cm/us All: West = 0.287066 +/- 0.00154489
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.548 +/- 8.07358e-06
  return (TDataSet *)tableSet;// West = 5.5478 +/- 8.78411e-06 East = 5.5491 +/- 2.04915e-05
};
