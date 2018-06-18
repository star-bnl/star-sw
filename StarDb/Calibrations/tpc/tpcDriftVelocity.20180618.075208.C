TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 169009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54634; // +/- 7.32181e-06 cm/us All: East = -0.395906 +/- 0.0046282
  row.laserDriftVelocityWest	 =   5.54634; // +/- 7.32181e-06 cm/us All: West = 0.290705 +/- 0.00136378
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54634 +/- 7.32181e-06
  return (TDataSet *)tableSet;// West = 5.54601 +/- 7.65846e-06 East = 5.5498 +/- 2.49699e-05
};
