TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54295; // +/- 1.85247e-05 cm/us All: East = 0.00751935 +/- 0.00376259
  row.laserDriftVelocityWest	 =   5.54295; // +/- 1.85247e-05 cm/us All: West = 0.71933 +/- 0.00815214
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54295 +/- 1.85247e-05
  return (TDataSet *)tableSet;// West = 5.53972 +/- 4.07805e-05 East = 5.54379 +/- 2.07939e-05
};
