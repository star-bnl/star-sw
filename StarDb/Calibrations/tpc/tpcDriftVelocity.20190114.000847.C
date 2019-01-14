TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 13066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54589; // +/- 1.61687e-05 cm/us All: East = -0.792674 +/- 0.00985069
  row.laserDriftVelocityWest	 =   5.54589; // +/- 1.61687e-05 cm/us All: West = -0.498295 +/- 0.00306627
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54589 +/- 1.61687e-05
  return (TDataSet *)tableSet;// West = 5.54575 +/- 1.69091e-05 East = 5.54734 +/- 5.52437e-05
};
