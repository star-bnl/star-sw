TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 118021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5388; // +/- 1.14728e-05 cm/us All: East = -0.236674 +/- 0.00758524
  row.laserDriftVelocityWest	 =   5.5388; // +/- 1.14728e-05 cm/us All: West = 0.303667 +/- 0.00214387
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5388 +/- 1.14728e-05
  return (TDataSet *)tableSet;// West = 5.53857 +/- 1.19977e-05 East = 5.54127 +/- 3.92165e-05
};
