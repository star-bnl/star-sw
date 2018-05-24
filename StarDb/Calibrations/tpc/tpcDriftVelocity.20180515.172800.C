TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54582; // +/- 5.00247e-06 cm/us All: East = 0.027484 +/- 0.0022908
  row.laserDriftVelocityWest	 =   5.54582; // +/- 5.00247e-06 cm/us All: West = 0.0407563 +/- 0.000953751
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54582 +/- 5.00247e-06
  return (TDataSet *)tableSet;// West = 5.54581 +/- 5.42691e-06 East = 5.54589 +/- 1.29033e-05
};
