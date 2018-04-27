TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 73033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55376; // +/- 9.7295e-06 cm/us All: East = -0.0140822 +/- 0.00678994
  row.laserDriftVelocityWest	 =   5.55376; // +/- 9.7295e-06 cm/us All: West = 0.163382 +/- 0.00184265
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55376 +/- 9.7295e-06
  return (TDataSet *)tableSet;// West = 5.55369 +/- 1.00649e-05 East = 5.55475 +/- 3.80064e-05
};
