TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 125040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55439; // +/- 6.41964e-06 cm/us All: East = 0.229032 +/- 0.00208058
  row.laserDriftVelocityWest	 =   5.55439; // +/- 6.41964e-06 cm/us All: West = 0.266778 +/- 0.00137738
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55439 +/- 6.41964e-06
  return (TDataSet *)tableSet;// West = 5.55433 +/- 7.70719e-06 East = 5.55453 +/- 1.16012e-05
};
