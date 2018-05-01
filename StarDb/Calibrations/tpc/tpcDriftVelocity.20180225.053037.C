TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 56002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5059; // +/- 1.34633e-05 cm/us All: East = 0.0662096 +/- 0.00382546
  row.laserDriftVelocityWest	 =   5.5059; // +/- 1.34633e-05 cm/us All: West = 0.267189 +/- 0.00324987
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5059 +/- 1.34633e-05
  return (TDataSet *)tableSet;// West = 5.5054 +/- 1.76458e-05 East = 5.50659 +/- 2.08273e-05
};
