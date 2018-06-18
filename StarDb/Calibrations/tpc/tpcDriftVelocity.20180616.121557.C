TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54686; // +/- 5.53141e-06 cm/us All: East = 0.218339 +/- 0.00222974
  row.laserDriftVelocityWest	 =   5.54686; // +/- 5.53141e-06 cm/us All: West = 0.162581 +/- 0.00109748
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54686 +/- 5.53141e-06
  return (TDataSet *)tableSet;// West = 5.54692 +/- 6.15154e-06 East = 5.54661 +/- 1.26416e-05
};
